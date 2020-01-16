module OpenTelemetry.LightStep.Exporter where

import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
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
