{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.Async
import Control.Monad.Catch
import Data.Function
import qualified Data.Text as T
import GHC.RTS.Events
import GHC.RTS.Events.Incremental
import GHC.Stack
import OpenTelemetry.Common hiding (Event, Timestamp)
import qualified OpenTelemetry.Common as OTel
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Exporter
import OpenTelemetry.LightStep.Config
import OpenTelemetry.LightStep.ZipkinExporter
import OpenTelemetry.SpanContext
import System.Clock
import System.Environment (getArgs, getEnvironment)
import System.FilePath
import System.IO
import System.Process.Typed
import qualified System.Random.SplitMix as R
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["read", path] -> do
      printf "Sending %s to LightStep...\n" path
      Just lsConfig <- getEnvConfig
      let service_name = T.pack $ takeBaseName path
      exporter <- createLightStepSpanExporter lsConfig {lsServiceName = service_name}
      origin_timestamp <- fromIntegral . toNanoSecs <$> getTime Realtime
      withFile path ReadMode (work origin_timestamp exporter)
      shutdown exporter
      putStrLn "\nAll done.\n"
    ("run" : program : "--" : args) -> do
      printf "Streaming eventlog of %s to LightStep...\n" program
      Just lsConfig <- getEnvConfig
      exporter <- createLightStepSpanExporter lsConfig {lsServiceName = T.pack program}
      let pipe = program <> "-opentelemetry.pipe"
      runProcess $ proc "mkfifo" [pipe]
      origin_timestamp <- fromIntegral . toNanoSecs <$> getTime Realtime
      bracket
        (async $ withFile pipe ReadMode (work origin_timestamp exporter))
        ( \restreamer -> do
            putStrLn "Cleanup"
            wait restreamer
            shutdown exporter
        )
        ( \_restreamer -> do
            env <- (("GHCRTS", "-l -ol" <> pipe) :) <$> getEnvironment
            runProcess (proc program args & setEnv env)
        )
      putStrLn "\nAll done.\n"
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-to-lightstep read <program.eventlog>"
      putStrLn "  eventlog-to-lightstep run <program> -- <program-args>"
