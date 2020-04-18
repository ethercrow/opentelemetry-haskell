{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Lightstep.Exporter where

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
import OpenTelemetry.Exporter
import OpenTelemetry.Lightstep.Config
import OpenTelemetry.SpanContext
import OpenTelemetry.ZipkinExporter
import Text.Printf

createLightstepSpanExporter :: MonadIO m => LightstepConfig -> m (Exporter Span)
createLightstepSpanExporter cfg@(LightstepConfig {..}) = liftIO do
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
