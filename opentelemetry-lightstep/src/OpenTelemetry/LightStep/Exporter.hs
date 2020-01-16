module OpenTelemetry.LightStepExporter where

import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
import OpenTelemetry.Common
import System.IO
import Text.Printf

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
