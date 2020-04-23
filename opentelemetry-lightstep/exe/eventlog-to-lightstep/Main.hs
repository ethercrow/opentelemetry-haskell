{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Exporter
import OpenTelemetry.Lightstep.Config
import OpenTelemetry.Lightstep.Exporter
import System.Clock
import System.Environment (getArgs)
import System.FilePath
import System.IO
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
      origin_timestamp <- fromIntegral . toNanoSecs <$> getTime Realtime
      withFile path ReadMode (work origin_timestamp exporter)
      shutdown exporter
      putStrLn "\nAll done.\n"
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-to-lightstep read <program.eventlog>"
