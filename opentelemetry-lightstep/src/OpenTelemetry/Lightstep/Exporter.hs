{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Lightstep.Exporter where

import Control.Monad.IO.Class
import OpenTelemetry.Common
import OpenTelemetry.Lightstep.Config
import OpenTelemetry.ZipkinExporter
import Text.Printf

createLightstepSpanExporter :: MonadIO m => LightstepConfig -> m (Exporter Span)
createLightstepSpanExporter LightstepConfig {..} = liftIO do
  let zcfg =
        ZipkinConfig
          { zEndpoint = printf "https://%s:%d/api/v2/spans" lsHostName (fromIntegral lsPort :: Int),
            zServiceName = lsServiceName,
            zGlobalTags =
              [ ("lightstep.component", lsServiceName),
                ("lightstep.access_token", lsToken)
              ],
            zSpanQueueSize = lsSpanQueueSize,
            zGracefulShutdownTimeoutSeconds = lsGracefulShutdownTimeoutSeconds
          }
  createZipkinSpanExporter zcfg
