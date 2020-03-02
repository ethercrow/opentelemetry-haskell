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
import Network.HTTP.Types
import OpenTelemetry.Common
import OpenTelemetry.Debug
import OpenTelemetry.LightStep.Config
import System.IO.Unsafe
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
            modifyTVar droppedSpanCountVar (\x -> x + length sps - q_vacancy)
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
  let endpoint = printf "https://%s:%d/api/v2/spans" lsHostName (fromIntegral lsPort :: Int)
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
            _ -> reportSpans endpoint manager cfg sps
          dd_ "must_shutdown" must_shutdown
          case must_shutdown of
            True -> pure ()
            False -> loop
    loop
  pure $! LightStepClient cfg sender q shutdown_var

reportSpans :: String -> Manager -> LightStepConfig -> [Span] -> IO ()
reportSpans endpoint httpManager cfg sps = do
  dd_ "reportSpans" sps
  let body = encode (map (ZipkinSpan cfg) sps)
      request =
        (parseRequest_ endpoint)
          { method = "POST",
            requestBody = RequestBodyLBS body,
            requestHeaders = [("Content-Type", "application/json")]
          }
  resp <- httpLbs request httpManager
  case statusCode (responseStatus resp) of
    200 -> do
      inc 1 reportedSpanCountVar
      pure ()
    _ -> do
      -- TODO(divanov): handle failures
      inc 1 rejectedSpanCountVar
      dd_ "body" body
      dd_ "resp status" $ responseStatus resp
      dd_ "resp" $ responseBody resp

droppedSpanCountVar :: TVar Int
droppedSpanCountVar = unsafePerformIO $ newTVarIO 0
{-# NOINLINE droppedSpanCountVar #-}

reportedSpanCountVar :: TVar Int
reportedSpanCountVar = unsafePerformIO $ newTVarIO 0
{-# NOINLINE reportedSpanCountVar #-}

rejectedSpanCountVar :: TVar Int
rejectedSpanCountVar = unsafePerformIO $ newTVarIO 0
{-# NOINLINE rejectedSpanCountVar #-}
