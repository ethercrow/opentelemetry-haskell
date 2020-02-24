{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.LightStep.ZipkinExporter where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import OpenTelemetry.Common
import OpenTelemetry.LightStep.Config
import Text.Printf

data ZipkinSpan
  = ZipkinSpan
      { zsToken :: T.Text,
        zsSpan :: Span
      }

instance ToJSON ZipkinSpan where
  -- FIXME(divanov): deduplicate
  toJSON (ZipkinSpan token s@(Span {..})) =
    let TId tid = spanTraceId s
        SId sid = spanId s
     in object
          [ "name" .= spanOperation,
            "traceId" .= T.pack (printf "%016x" tid),
            "id" .= T.pack (printf "%016x" sid),
            "kind" .= String "SERVER",
            "timestamp" .= spanStartedAt,
            "duration" .= (spanFinishedAt - spanStartedAt),
            "tags" .= object ["lightstep.access_token" .= token]
          ]
  toEncoding (ZipkinSpan token s@(Span {..})) =
    let TId tid = spanTraceId s
        SId sid = spanId s
     in pairs
          ( "name" .= spanOperation
              <> "traceId" .= T.pack (printf "%016x" tid)
              <> "id" .= T.pack (printf "%016x" sid)
              <> "kind" .= String "CLIENT"
              <> "timestamp" .= (spanStartedAt `div` 1000)
              <> "duration" .= ((spanFinishedAt - spanStartedAt) `div` 1000)
              <> "tags" .= object ["lightstep.access_token" .= token]
          )

data LightStepClient
  = LightStepClient
      { lscHttpManager :: Manager,
        lscConfig :: LightStepConfig
      }

d_ :: String -> IO ()
d_ = putStrLn

createLightStepSpanExporter :: LightStepConfig -> IO (Exporter Span)
createLightStepSpanExporter cfg = do
  client <- mkClient cfg
  pure
    $! Exporter
      ( \sps -> do
          reportSpans client sps
          pure ExportSuccess
      )
      (pure ())

mkClient :: LightStepConfig -> IO LightStepClient
mkClient lscConfig@(LightStepConfig {..}) = do
  manager <- newManager tlsManagerSettings
  pure $! LightStepClient manager lscConfig

reportSpans :: LightStepClient -> [Span] -> IO ()
reportSpans client@(LightStepClient {..}) sps = do
  let -- TODO(divanov) unhardcode endpoint
      url = "https://ingest.lightstep.com:443/api/v2/spans"
      body = encode (map (ZipkinSpan (lsToken lscConfig)) sps)
      request =
        (parseRequest_ url)
          { method = "POST",
            requestBody = RequestBodyLBS body,
            requestHeaders = [("Content-Type", "application/json")]
          }
  -- TODO(divanov): count reported and rejected spans
  -- TODO(divanov): handle failures
  BSL.putStrLn body
  resp <- httpLbs request lscHttpManager
  print resp
  print (responseBody resp)
  pure ()
