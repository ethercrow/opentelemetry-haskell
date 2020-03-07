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
import Network.HTTP.Types (status200)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import OpenTelemetry.Common
import OpenTelemetry.FileExporter
import OpenTelemetry.Implicit
import OpenTelemetry.LightStep.Config
import OpenTelemetry.LightStep.ZipkinExporter
import qualified OpenTelemetry.Network.HTTP.Client as HTTPClientTelemetry
import qualified OpenTelemetry.Network.Wai.Middleware as WaiTelemetry
import OpenTelemetry.Propagation
import System.Environment
import System.Exit
import Text.Printf

megaport :: Int
megaport = 6502

main :: IO ()
main = do
  args <- tail <$> getArgs
  exporter <- case args of
    ["--file", f] -> createFileSpanExporter f
    ["--lightstep"] -> do
      Just cfg <- getEnvConfig
      createLightStepSpanExporter cfg
    _ -> do
      putStrLn "Usage:"
      putStrLn "  opentelemetry-megaexample [--lightstep] [--file FILE]"
      putStrLn ""
      printf "curl http://localhost:%d/http/example.com\n"
      exitSuccess
  let otConfig =
        OpenTelemetryConfig
          { otcSpanExporter = exporter
          }
  withOpenTelemetry otConfig $ seriousPragmaticMain

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
  let hdrs = Wai.requestHeaders req
  case extractSpanContextFromHeaders hdrs of
    Just c -> do
      setParentSpanContext c
    Nothing -> do
      pure ()
  case Wai.pathInfo req of
    ("http" : rest) -> do
      let target = "http://" <> T.intercalate "/" rest
      result <- get target
      respond $ Wai.responseLBS status200 [] result
    _ -> do
      Just sp <- getCurrentActiveSpan
      bg_work <- async $ withChildSpanOf sp "background_task" do
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
  manager <- withSpan "newManager" $ newManager (HTTPClientTelemetry.middleware defaultManagerSettings)
  resp <- httpLbs request manager
  pure $ responseBody resp
