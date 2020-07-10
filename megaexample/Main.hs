{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Function
import Data.String
import qualified Data.Text as T
import GHC.Stats
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types (status200, statusCode)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import OpenTelemetry.Eventlog
import qualified OpenTelemetry.Network.Wai.Middleware as WaiTelemetry
import OpenTelemetry.Propagation
import OpenTelemetry.SpanContext
import System.Environment
import System.Mem
import System.Random (randomIO)
import Text.Printf

megaport :: Int
megaport = 6502

main :: IO ()
main = do
  args <- getArgs
  ghcrts <- lookupEnv "GHCRTS"
  printf "GHCRTS = %s\n" (show ghcrts)
  case "--interactive" `elem` args of
    True -> serverMain
    False -> race_ serverMain clientMain

serverMain :: IO ()
serverMain = withSpan_ "serverMain" $ do
  let settings =
        Warp.defaultSettings
          & Warp.setPort megaport
          & Warp.setHost "127.0.0.1"
  printf "Listening on 127.0.0.1:%d\n" megaport

  httpRequestCounter <- mkCounter "http_requests"
  telemetryMiddleware <- WaiTelemetry.mkMiddleware
  let server = telemetryMiddleware $ microservice httpRequestCounter
  Warp.runSettings settings server

clientMain :: IO ()
clientMain = withSpan "clientMain" $ \sp -> do
  trace_id <- TId <$> randomIO
  setTraceId sp trace_id
  span_id <- SId <$> randomIO
  setSpanId sp span_id
  threadDelay 100000
  let propagationHeaders = propagateToHeaders w3cTraceContext $ SpanContext span_id trace_id
  manager <- withSpan_ "newManager" $ newManager defaultManagerSettings
  _ <- httpLbs ((fromString $ printf "http://127.0.0.1:%d/http/127.0.0.1:%d/haskell.org" megaport megaport) {requestHeaders = propagationHeaders}) manager
  _ <- httpLbs ((fromString $ printf "http://127.0.0.1:%d/http/127.0.0.1:%d/gc" megaport megaport) {requestHeaders = propagationHeaders}) manager
  resp <- httpLbs ((fromString $ printf "http://127.0.0.1:%d/http/127.0.0.1:%d/stuff" megaport megaport) {requestHeaders = propagationHeaders}) manager
  print resp

microservice :: Counter -> Wai.Application
microservice httpRequestCounter = \req respond -> withSpan "handle_http_request" $ \sp -> do
  my_trace_id <- case propagateFromHeaders w3cTraceContext (Wai.requestHeaders req) of
    Nothing -> do
      trace_id <- TId <$> randomIO
      setTraceId sp trace_id
      pure trace_id
    Just ctx@(SpanContext _span_id trace_id) -> do
      setParentSpanContext sp ctx
      pure trace_id

  case Wai.pathInfo req of
    ["gc"] -> do
      performGC
      respond $ Wai.responseLBS status200 [] ""
    ("http" : rest) -> do
      add httpRequestCounter 1
      let target = "http://" <> T.intercalate "/" rest
      result <- get my_trace_id target
      respond $ Wai.responseLBS status200 [] result
    _ -> do
      bg_work <- async $ withSpan_ "background_task" do
        threadDelay 10000
        pure ()
      addEvent sp "message" "started bg work"
      rtsStats <- withSpan_ "getRTSStats" getRTSStats
      () <- wait bg_work
      addEvent sp "message" "finished bg work"
      respond $
        Wai.responseLBS
          status200
          [("Content-Type", "text/plain")]
          ( LBS.pack $
              unlines
                [ printf "RAM usage: %d bytes" (max_live_bytes rtsStats)
                ]
          )

get :: TraceId -> T.Text -> IO LBS.ByteString
get trace_id (T.unpack -> url) = withSpan "call_http_get" $ \sp -> do
  span_id <- SId <$> randomIO
  setSpanId sp span_id

  let request = (fromString url) {requestHeaders = propagationHeaders}
      propagationHeaders = propagateToHeaders w3cTraceContext $ SpanContext span_id trace_id
  -- A new manager is created for every request
  -- so that it's visible in the trace how much this hurts performance.
  manager <- withSpan_ "newManager" $ newManager tlsManagerSettings

  resp <- httpLbs request manager
  setTag sp "http.status_code" (BS.pack (printf "%d" (statusCode $ responseStatus resp)))
  pure $ responseBody resp
