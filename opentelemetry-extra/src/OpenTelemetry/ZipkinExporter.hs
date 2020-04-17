{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OpenTelemetry.ZipkinExporter where

-- Zipkin V2 protocol spec: https://github.com/openzipkin/zipkin-api/blob/master/zipkin2-api.yaml

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
import OpenTelemetry.Exporter
import OpenTelemetry.SpanContext
import System.IO.Unsafe
import Text.Printf

data ZipkinSpan = ZipkinSpan
  { zsConfig :: ZipkinConfig,
    zsSpan :: Span
  }

tagValue2text :: TagValue -> T.Text
tagValue2text tv = case tv of
  (StringTagValue s) -> s
  (BoolTagValue b) -> if b then "true" else "false"
  (IntTagValue i) -> T.pack $ show i
  (DoubleTagValue d) -> T.pack $ show (fromFloatDigits d)

instance ToJSON ZipkinSpan where
  -- FIXME(divanov): deduplicate
  toJSON (ZipkinSpan ZipkinConfig {..} s@(Span {..})) =
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
            "localEndpoint" .= object ["serviceName" .= zServiceName],
            "tags"
              .= object
                ( [k .= v | (k, v) <- zGlobalTags]
                    <> [k .= tagValue2text v | (k, v) <- HM.toList spanTags]
                ),
            "annotations"
              .= [ object ["timestamp" .= (t `div` 1000), "value" .= v]
                   | SpanEvent t _ v <- spanEvents
                 ]
          ]
            <> (maybe [] (\(SId psid) -> ["parentId" .= psid]) spanParentId)

  toEncoding (ZipkinSpan ZipkinConfig {..} s@(Span {..})) =
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
              <> "localEndpoint" .= object ["serviceName" .= zServiceName]
              <> "tags"
                .= object
                  ( [k .= v | (k, v) <- zGlobalTags]
                      <> [k .= tagValue2text v | (k, v) <- HM.toList spanTags]
                  )
              <> ( maybe
                     mempty
                     (\(SId psid) -> "parentId" .= T.pack (printf "%016x" psid))
                     spanParentId
                 )
              <> "annotations"
                .= [ object ["timestamp" .= (t `div` 1000), "value" .= v]
                     | SpanEvent t _ v <- spanEvents
                   ]
          )

data ZipkinConfig = ZipkinConfig
  { zEndpoint :: String,
    zServiceName :: T.Text,
    zGlobalTags :: [(T.Text, T.Text)],
    zGracefulShutdownTimeoutSeconds :: Word,
    zSpanQueueSize :: Word
  }

localhostZipkinConfig :: T.Text -> ZipkinConfig
localhostZipkinConfig service =
  ZipkinConfig
    { zEndpoint = "http://localhost:9411/api/v2/spans",
      zServiceName = service,
      zGlobalTags = mempty,
      zGracefulShutdownTimeoutSeconds = 5,
      zSpanQueueSize = 2048
    }

data ZipkinClient = ZipkinClient
  { zcConfig :: ZipkinConfig,
    zcSenderThread :: Async (),
    zcSenderQueue :: TBQueue Span,
    zcShutdownVar :: TVar Bool
  }

createZipkinSpanExporter :: MonadIO m => ZipkinConfig -> m (Exporter Span)
createZipkinSpanExporter cfg = liftIO do
  client <- mkClient cfg
  pure
    $! Exporter
      ( \sps -> do
          let q = zcSenderQueue client
          atomically $ do
            q_population <- fromIntegral <$> lengthTBQueue q
            let q_vacancy = fromIntegral (zSpanQueueSize (zcConfig client) - q_population)
            modifyTVar droppedSpanCountVar (\x -> x + length sps - q_vacancy)
            mapM_
              (writeTBQueue q)
              (take q_vacancy sps)
          pure ExportSuccess
      )
      ( do
          atomically $
            writeTVar (zcShutdownVar client) True
          wait (zcSenderThread client)
      )

mkClient :: ZipkinConfig -> IO ZipkinClient
mkClient cfg@(ZipkinConfig {..}) = do
  manager <- newManager tlsManagerSettings
  q <- newTBQueueIO (fromIntegral zSpanQueueSize)
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
            _ -> reportSpans zEndpoint manager cfg sps
          dd_ "must_shutdown" must_shutdown
          case must_shutdown of
            True -> pure ()
            False -> loop
    loop
  pure $! ZipkinClient cfg sender q shutdown_var

reportSpans :: String -> Manager -> ZipkinConfig -> [Span] -> IO ()
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
      dd_ @String "200" "200"
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
