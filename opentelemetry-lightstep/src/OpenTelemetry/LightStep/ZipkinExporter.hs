{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.LightStep.ZipkinExporter where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Scientific
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import OpenTelemetry.Common
import OpenTelemetry.LightStep.Config
import Text.Printf

data ZipkinSpan
  = ZipkinSpan
      { zsConfig :: LightStepConfig,
        zsSpan :: Span
      }

tagValue2json :: TagValue -> Value
tagValue2json tv = case tv of
  (StringTagValue s) -> String s
  (BoolTagValue b) -> Bool b
  (IntTagValue i) -> Number (fromIntegral i)
  (DoubleTagValue d) -> Number (fromFloatDigits d)

instance ToJSON ZipkinSpan where
  -- FIXME(divanov): deduplicate
  toJSON (ZipkinSpan LightStepConfig {..} s@(Span {..})) =
    let TId tid = spanTraceId s
        SId sid = spanId s
        ts = spanStartedAt `div` 1000
        duration = (spanFinishedAt - spanStartedAt) `div` 1000
     in object $
          [ "name" .= spanOperation,
            "traceId" .= T.pack (printf "%016x" tid),
            "id" .= T.pack (printf "%016x" sid),
            "timestamp" .= ts,
            "duration" .= duration,
            "tags"
              .= object
                ( [ "lightstep.access_token" .= lsToken,
                    "lightstep.component_name" .= lsServiceName
                  ]
                    <> [k .= v | (k, v) <- lsGlobalTags]
                    <> [k .= tagValue2json v | (k, v) <- HM.toList spanTags]
                )
          ]
            <> (maybe [] (\(SId psid) -> ["parentId" .= psid]) spanParentId)
  toEncoding (ZipkinSpan LightStepConfig {..} s@(Span {..})) =
    let TId tid = spanTraceId s
        SId sid = spanId s
        ts = spanStartedAt `div` 1000
        duration = (spanFinishedAt - spanStartedAt) `div` 1000
     in pairs
          ( "name" .= spanOperation
              <> "traceId" .= T.pack (printf "%016x" tid)
              <> "id" .= T.pack (printf "%016x" sid)
              <> "timestamp" .= ts
              <> "duration" .= duration
              <> "tags"
                .= object
                  ( [ "lightstep.access_token" .= lsToken,
                      "lightstep.component_name" .= lsServiceName
                    ]
                      <> [k .= v | (k, v) <- lsGlobalTags]
                      <> [k .= tagValue2json v | (k, v) <- HM.toList spanTags]
                  )
              <> ( maybe
                     mempty
                     (\(SId psid) -> "parentId" .= T.pack (printf "%016x" psid))
                     spanParentId
                 )
          )

data LightStepClient
  = LightStepClient
      { lscConfig :: LightStepConfig,
        lscSenderThread :: Async (),
        lscSenderQueue :: TBQueue Span,
        lscShutdownVar :: TVar Bool
      }

d_ :: Show a => a -> IO ()
d_ thing = do
  -- TODO(divanov): make it print thing or not print thing based on OPENTELEMETRY_DEBUG env var
  pure ()

createLightStepSpanExporter :: MonadIO m => LightStepConfig -> m (Exporter Span)
createLightStepSpanExporter cfg = liftIO do
  client <- mkClient cfg
  pure
    $! Exporter
      ( \sps -> do
          let q = lscSenderQueue client
          atomically $ do
            q_population <- fromIntegral <$> lengthTBQueue q
            let q_vacancy = fromIntegral (lsSpanQueueSize (lscConfig client) - q_population)
            -- TODO(divanov): increment dropped span counter by (len sps - q_vacancy)
            mapM_
              (writeTBQueue q)
              (take q_vacancy sps)
          pure ExportSuccess
      )
      ( do
          atomically $
            writeTVar (lscShutdownVar client) True
          wait (lscSenderThread client)
      )

mkClient :: LightStepConfig -> IO LightStepClient
mkClient cfg@(LightStepConfig {..}) = do
  manager <- newManager tlsManagerSettings
  q <- newTBQueueIO (fromIntegral lsSpanQueueSize)
  shutdown_var <- newTVarIO False
  sender <- async $ do
    let loop = do
          (must_shutdown, sps) <- atomically $ do
            must_shutdown <- readTVar shutdown_var
            sps <- flushTBQueue q
            case (must_shutdown, sps) of
              (False, []) -> retry
              _ -> pure (must_shutdown, sps)
          case sps of
            [] -> pure ()
            _ -> reportSpans manager cfg sps
          d_ ("must_shutdown", must_shutdown)
          case must_shutdown of
            True -> pure ()
            False -> loop
    loop
  pure $! LightStepClient cfg sender q shutdown_var

reportSpans :: Manager -> LightStepConfig -> [Span] -> IO ()
reportSpans httpManager cfg sps = do
  d_ ("reportSpans", sps)
  let -- TODO(divanov) unhardcode endpoint
      url = "https://ingest.lightstep.com:443/api/v2/spans"
      body = encode (map (ZipkinSpan cfg) sps)
      request =
        (parseRequest_ url)
          { method = "POST",
            requestBody = RequestBodyLBS body,
            requestHeaders = [("Content-Type", "application/json")]
          }
  -- TODO(divanov): count reported and rejected spans
  -- TODO(divanov): handle failures
  d_ ("body", body)
  resp <- httpLbs request httpManager
  d_ ("resp status", responseStatus resp)
  d_ ("resp", responseBody resp)
  pure ()
