{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.LightStep.Exporter where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ProtoLens.Message (defMessage)
import qualified Data.Text as T
import Data.Version (showVersion)
import Network.GRPC.Client
import Network.GRPC.Client.Helpers
import Network.GRPC.HTTP2.ProtoLens
import Network.HTTP2.Client
import OpenTelemetry.Common
import OpenTelemetry.LightStep.Config
import Paths_opentelemetry_lightstep (version)
import qualified Proto.Collector as P
import Proto.Collector_Fields hiding (spanContext, spanId)
import qualified Proto.Collector_Fields as P
import Proto.Google.Protobuf.Timestamp_Fields
import System.Timeout

data LightStepClient
  = LightStepClient
      { lscGrpcVar :: MVar GrpcClient,
        lscReporter :: P.Reporter,
        lscConfig :: LightStepConfig
      }

d_ :: String -> IO ()
d_ = putStrLn

createLightStepExporter :: LightStepConfig -> IO (Exporter Span)
createLightStepExporter cfg = do
  client <- mkClient cfg
  pure
    $! Exporter
      ( \sps -> do
          reportSpans client sps
          pure ExportSuccess
      )
      ( do
          _ <- closeClient client
          pure ()
      )

mkClient :: LightStepConfig -> IO LightStepClient
mkClient lscConfig@(LightStepConfig {..}) = do
  grpc <- makeGrpcClient lscConfig
  lscGrpcVar <- newMVar grpc
  let lscReporter =
        defMessage
          & reporterId .~ 2
          & tags
            .~ ( [ defMessage & key .~ "lightstep.component_name" & stringValue .~ lsServiceName,
                   defMessage & key .~ "lightstep.tracer_platform" & stringValue .~ "haskell",
                   defMessage & key .~ "lightstep.tracer_version" & stringValue .~ (T.pack $ showVersion version)
                 ]
                   <> [ defMessage & key .~ k & stringValue .~ v
                        | (k, v) <- lsGlobalTags
                      ]
               )
  pure LightStepClient {..}

convertSpan :: Span -> P.Span
convertSpan s@(Span {..}) =
  defMessage
    & operationName .~ spanOperation
    & startTimestamp
      .~ ( defMessage
             & seconds .~ fromIntegral (spanStartedAt `div` 1_000_000_000)
             & nanos .~ fromIntegral (rem spanStartedAt 1_000_000_000)
         )
    & P.spanContext
      .~ ( defMessage
             & traceId .~ tid
             & P.spanId .~ sid
         )
  where
    TId tid = (spanTraceId s)
    SId sid = spanId s

closeClient :: LightStepClient -> IO (Either ClientError ())
closeClient LightStepClient {..} = readMVar lscGrpcVar >>= runExceptT . close

reportSpans :: LightStepClient -> [Span] -> IO ()
reportSpans client@(LightStepClient {..}) (map convertSpan -> sps) = do
  let LightStepConfig {..} = lscConfig
  let tryOnce = do
        let req ::
              IO
                ( Maybe
                    ( Either
                        ClientError
                        (Either TooMuchConcurrency (RawReply P.ReportResponse))
                    )
                )
            req =
              timeout 3_000_000 $ do
                grpc <- readMVar lscGrpcVar
                runExceptT $ do
                  rawUnary
                    (RPC :: RPC P.CollectorService "report")
                    grpc
                    ( defMessage
                        & auth .~ (defMessage & accessToken .~ lsToken)
                        & spans .~ sps
                        & reporter .~ lscReporter
                    )
        fst
          <$> generalBracket
            (pure ())
            ( \_ -> \case
                ExitCaseException err -> do
                  d_ $ "reportSpans failed: " <> show (err :: SomeException)
                ExitCaseAbort -> do
                  d_ $ "reportSpans aborted"
                ExitCaseSuccess _ -> pure ()
            )
            (\_ -> req)
  ret <- tryOnce
  ret2 <- case ret of
    Nothing -> do
      d_ "GRPC client is stuck, trying to reconnect"
      reconnectClient client
      -- one retry after reconnect
      tryOnce
    _ -> pure ret
  case ret2 of
    Nothing -> pure ()
    _ -> pure ()
  -- d_ $ show ret2
  pure ()

reconnectClient :: LightStepClient -> IO ()
reconnectClient LightStepClient {..} = do
  d_ "reconnectClient begin"
  -- inc 1 reconnectCountVar
  newGrpc <- makeGrpcClient lscConfig
  oldGrpc <- swapMVar lscGrpcVar newGrpc
  _ <- runExceptT $ close oldGrpc
  d_ "reconnectClient end"

makeGrpcClient :: LightStepConfig -> IO GrpcClient
makeGrpcClient LightStepConfig {..} = do
  newGrpcOrError <-
    runExceptT $
      setupGrpcClient
        ( (grpcClientConfigSimple lsHostName lsPort True)
            { _grpcClientConfigCompression = compression,
              _grpcClientConfigTimeout = Timeout 5, -- seconds
              _grpcClientConfigGoAwayHandler = \_ -> liftIO $ putStrLn "GoAway handler fired"
            }
        )
  case newGrpcOrError of
    Right newGrpc -> pure newGrpc
    Left err -> throwM err
  where
    compression = if False then gzip else uncompressed
