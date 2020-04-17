{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Monad.Catch
import Data.Function
import qualified Data.Text as T
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Exporter
import OpenTelemetry.ZipkinExporter
import System.Clock
import System.Environment (getArgs, getEnvironment)
import System.FilePath
import System.IO
import System.Process.Typed
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["read", path] -> do
      printf "Sending %s to Zipkin...\n" path
      let service_name = T.pack $ takeBaseName path
      exporter <- createZipkinSpanExporter $ localhostZipkinConfig service_name
      origin_timestamp <- fromIntegral . toNanoSecs <$> getTime Realtime
      withFile path ReadMode (work origin_timestamp exporter)
      shutdown exporter
      putStrLn "\nAll done.\n"
    ("run" : program : "--" : args') -> do
      printf "Streaming eventlog of %s to Zipkin...\n" program
      exporter <- createZipkinSpanExporter $ localhostZipkinConfig (T.pack program)
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
            runProcess (proc program args' & setEnv env)
        )
      putStrLn "\nAll done.\n"
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-to-lightstep read <program.eventlog>"
      putStrLn "  eventlog-to-lightstep run <program> -- <program-args>"
