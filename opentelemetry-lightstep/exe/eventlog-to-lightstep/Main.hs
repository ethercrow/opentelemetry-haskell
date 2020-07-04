{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Lightstep.Config
import OpenTelemetry.Common
import OpenTelemetry.Lightstep.Exporter
import System.Environment (getArgs)
import System.FilePath
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["read", path] -> do
      printf "Sending %s to Lightstep...\n" path
      Just lsConfig <- getEnvConfig
      let service_name = T.pack $ takeBaseName path
      span_exporter <- createLightstepSpanExporter lsConfig {lsServiceName = service_name}
      let metric_exporter = noopExporter
      exportEventlog span_exporter metric_exporter path
      shutdown span_exporter
      shutdown metric_exporter
      putStrLn "\nAll done."
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-to-lightstep read <program.eventlog>"
