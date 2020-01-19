module OpenTelemetry.LightStep.Exporter where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
import Network.GRPC.Client
import Network.GRPC.Client.Helpers
import Network.GRPC.HTTP2.ProtoLens
import Network.HTTP2.Client
import OpenTelemetry.Common
import OpenTelemetry.LightStep.Config
import System.IO

createLightStepExporter :: LightStepConfig -> IO (Exporter Span)
createLightStepExporter LightStepConfig {..} = do
  pure
    $! Exporter
      ( \sps -> do
          pure ExportSuccess
      )
      ( do
          pure ()
      )

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
