{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Exporter
import OpenTelemetry.ZipkinExporter
import System.Environment (getArgs)
import System.FilePath
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["read", path] -> do
      printf "Sending %s to Zipkin...\n" path
      let service_name = T.pack $ takeBaseName path
      span_exporter <- createZipkinSpanExporter $ localhostZipkinConfig service_name
      let metric_exporter = noopExporter
      exportEventlog span_exporter metric_exporter path
      shutdown span_exporter
      putStrLn "\nAll done."
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-to-zipkin read <program.eventlog>"
