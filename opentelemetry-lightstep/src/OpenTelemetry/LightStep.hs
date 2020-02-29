module OpenTelemetry.LightStep where

import Control.Monad.Catch
import Control.Monad.IO.Class
import OpenTelemetry.Common
import OpenTelemetry.Implicit
import OpenTelemetry.LightStep.Config
import OpenTelemetry.LightStep.ZipkinExporter
import System.IO

withEnvConfigLightStepOpenTelemetry :: (MonadIO m, MonadMask m) => m a -> m a
withEnvConfigLightStepOpenTelemetry action = do
  mcfg <- getEnvConfig
  case mcfg of
    Just cfg -> do
      exporter <- createLightStepSpanExporter cfg
      let otelConfig = OpenTelemetryConfig {otcSpanExporter = exporter}
      withOpenTelemetry otelConfig action
    Nothing -> do
      liftIO $ hPutStrLn stderr "Warning: LightStep exporter is not configured"
      action
