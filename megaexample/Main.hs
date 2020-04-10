{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Function
import Data.String
import qualified Data.Text as T
import GHC.Stats
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types (status200)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import OpenTelemetry.Eventlog
import qualified OpenTelemetry.Network.Wai.Middleware as WaiTelemetry
import System.Mem
import Text.Printf

megaport :: Int
megaport = 6502

main :: IO ()
main = seriousPragmaticMain

seriousPragmaticMain :: IO ()
seriousPragmaticMain = do
  let settings =
        Warp.defaultSettings
          & Warp.setPort megaport
          & Warp.setHost "127.0.0.1"
  printf "Listening on 127.0.0.1:%d\n" megaport
  Warp.runSettings settings (WaiTelemetry.middleware microservice)

microservice :: Wai.Application
microservice = \req respond -> withSpan "handle_http_request" $ do
  case Wai.pathInfo req of
    ["gc"] -> do
      performGC
      respond $ Wai.responseLBS status200 [] ""
    ("http" : rest) -> do
      let target = "http://" <> T.intercalate "/" rest
      result <- get target
      respond $ Wai.responseLBS status200 [] result
    _ -> do
      bg_work <- async $ withSpan "background_task" do
        threadDelay 10000
        pure ()
      addEvent "message" "started bg work"
      rtsStats <- withSpan "getRTSStats" getRTSStats
      () <- wait bg_work
      addEvent "message" "finished bg work"
      respond $
        Wai.responseLBS
          status200
          [("Content-Type", "text/plain")]
          ( LBS.pack $
              unlines
                [ "RAM usage: " <> show (max_live_bytes rtsStats `div` 1_000) <> " KB"
                ]
          )

get :: T.Text -> IO LBS.ByteString
get (T.unpack -> url) = withSpan "call_http_get" $ do
  let request = fromString url
  -- A new manager is created for every request
  -- so that it's visible in the trace how much this hurts performance.
  manager <- withSpan "newManager" $ newManager tlsManagerSettings
  resp <- httpLbs request manager
  pure $ responseBody resp
