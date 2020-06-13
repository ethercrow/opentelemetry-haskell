{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Lightstep.Config
import OpenTelemetry.Exporter
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
      exporter <- createLightstepSpanExporter lsConfig {lsServiceName = service_name}
      exportEventlog exporter path
      shutdown exporter
      putStrLn "\nAll done."
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-to-lightstep read <program.eventlog>"
