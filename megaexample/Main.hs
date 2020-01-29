{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (find)
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
import OpenTelemetry.LightStep.Exporter
import OpenTelemetry.Propagation
import System.Environment
import System.Exit

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
      putStrLn "curl http://localhost:6502/http/example.com"
      exitSuccess
  let otConfig =
        OpenTelemetryConfig
          { otcSpanExporter = exporter
          }
  withOpenTelemetry otConfig $ seriousPragmaticMain

seriousPragmaticMain :: IO ()
seriousPragmaticMain = do
  Warp.run 6502 microservice

microservice :: Wai.Application
microservice = \req respond -> withSpan "handle_http_request" $ do
  let hdrs = Wai.requestHeaders req
  case extractSpanContextFromHeaders hdrs of
    _ -> pure ()
  case Wai.pathInfo req of
    ("http" : rest) -> do
      let target = "http://" <> T.intercalate "/" rest
      result <- get target
      respond $ Wai.responseLBS status200 [] result
    _ -> do
      rtsStats <- withSpan "getRTSStats" getRTSStats
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
  setTag @String "span.kind" "client"
  setTag "http.url" url
  let request =
        fromString url
  manager <- withSpan "newManager" $ newManager defaultManagerSettings
  resp <- httpLbs request manager
  setTag "http.status" $ show (responseStatus resp)
  pure $ responseBody resp
