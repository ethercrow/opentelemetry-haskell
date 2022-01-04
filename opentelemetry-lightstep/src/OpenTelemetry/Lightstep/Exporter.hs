{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OpenTelemetry.Lightstep.Exporter where

import qualified Data.HashMap.Strict as HM
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Scientific
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import OpenTelemetry.Common
import OpenTelemetry.Debug
import OpenTelemetry.Lightstep.Config
import OpenTelemetry.SpanContext
import System.IO.Unsafe
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Jsonifier as J

data LightstepSpan = LightstepSpan
  { lsConfig :: LightstepConfig,
    lsSpan :: Span
  }

tagValue2text :: TagValue -> J.Json
tagValue2text tv = J.textString $ case tv of
  (StringTagValue (TagVal s)) -> s
  (BoolTagValue b) -> if b then "true" else "false"
  (IntTagValue i) -> T.pack $ show i
  (DoubleTagValue d) -> T.pack $ show (fromFloatDigits d)

jRuntime :: Timestamp -> LightstepConfig -> J.Json
jRuntime ts LightstepConfig {lsServiceName} = J.object
  [ ("guid", J.textString $ T.pack (printf "%016x" ts))
  , ("start_micros", J.wordNumber $ fromIntegral ts)
  , ("group_name", J.textString lsServiceName)
  , ("attrs", J.array
      [J.object
        [("key", J.textString "lightstep.tracer_platform")
        ,("value", J.textString "java")
        ]
      ,J.object
        [("key", J.textString "lightstep.tracer_version")
        ,("value", J.textString "1.0")
        ]
      ,J.object
        [("key", J.textString "lightstep.tracer_platform_version")
        ,("value", J.textString "1.0")
        ]
      ])
  ]

jKV :: T.Text -> J.Json -> J.Json
jKV k v = J.object [("Key", J.textString k), ("Value", v)]

jSpan :: LightstepConfig -> Span -> J.Json
jSpan LightstepConfig {..} s@(Span {..}) =
  let TId tid = spanTraceId s
      SId sid = spanId s
   in J.object $
        [ ("span_name", J.textString spanOperation),
          ("span_guid", J.textString $ T.pack (printf "%016x" sid)),
          ("trace_guid", J.textString $ T.pack (printf "%016x" tid)),
          ("oldest_micros", J.wordNumber $ fromIntegral $ spanStartedAt `div` 1000),
          ("youngest_micros", J.wordNumber $ fromIntegral $ spanFinishedAt `div` 1000),
          ("attributes", J.array $ concat
            [ [jKV k (J.textString v) | (k, v) <- lsGlobalTags]
            , [jKV k (tagValue2text v) | ((TagName k), v) <- HM.toList spanTags]
            , (maybe [] (\(SId psid) ->
                  [ jKV "parent_span_guid" (J.textString $ T.pack (printf "%016x" psid))
                  ])
                spanParentId)
            ]
          )
        ]

data LightstepClient = LightstepClient
  { lscConfig :: LightstepConfig,
    lscSenderThread :: Async (),
    lscSenderQueue :: TBQueue Span,
    lscShutdownVar :: TVar Bool
  }

createLightstepSpanExporter :: MonadIO m => LightstepConfig -> m (Exporter Span)
createLightstepSpanExporter cfg@(LightstepConfig {..}) = liftIO do
  client <- mkClient cfg
  pure
    $! Exporter
      ( \sps -> do
          let q = lscSenderQueue client
          atomically $ do
            q_population <- fromIntegral <$> lengthTBQueue q
            let q_vacancy = fromIntegral (lsSpanQueueSize - q_population)
            modifyTVar' droppedSpanCountVar (\x -> x + length sps - q_vacancy)
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

mkClient :: LightstepConfig -> IO LightstepClient
mkClient cfg@(LightstepConfig {..}) = do
  manager <- newManager tlsManagerSettings
  q <- newTBQueueIO (fromIntegral lsSpanQueueSize)
  shutdown_var <- newTVarIO False
  ts <- now64
  let runtime = jRuntime ts cfg
      endpoint = printf "https://%s:%d/api/v0/reports" lsHostName (fromIntegral lsPort :: Int)
      req_template = parseRequest_ endpoint
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
            _ -> reportSpans runtime req_template manager cfg sps
          dd_ "must_shutdown" must_shutdown
          case must_shutdown of
            True -> pure ()
            False -> loop
    loop
  pure $! LightstepClient cfg sender q shutdown_var

reportSpans :: J.Json -> Request -> Manager -> LightstepConfig -> [Span] -> IO ()
reportSpans runtime req_template httpManager cfg sps = do
  dd_ "reportSpans" sps
  let !body = J.toByteString $ J.object
        [ ("runtime", runtime)
        , ("oldest_micros", J.wordNumber $ fromIntegral (minimum (map spanStartedAt sps)))
        , ("youngest_micros", J.wordNumber $ fromIntegral (maximum (map spanFinishedAt sps)))
        , ("span_records", J.array (map (jSpan cfg) sps))
        , ("internal_metrics", J.object [("counts", J.array [J.object [("name", J.textString "spans.dropped"), ("int64_value", J.wordNumber 0)]])])
        ]
      request =
        req_template
          { method = "POST",
            requestBody = RequestBodyBS body,
            requestHeaders =
              [ ("Lightstep-Access-Token", TE.encodeUtf8 (lsToken cfg))
              , ("Content-Type", "application/json")
              ]
          }
  resp <- httpLbs request httpManager
  case statusCode (responseStatus resp) of
    s | s `elem` [200, 202] -> do
      inc 1 reportedSpanCountVar
      pure ()
    _ -> do
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
