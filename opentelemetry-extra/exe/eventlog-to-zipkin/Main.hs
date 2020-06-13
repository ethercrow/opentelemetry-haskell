{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Exporter
import OpenTelemetry.ZipkinExporter
import System.Clock
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
      exporter <- createZipkinSpanExporter $ localhostZipkinConfig service_name
      origin_timestamp <- fromIntegral . toNanoSecs <$> getTime Realtime
      work origin_timestamp exporter $ EventLogFilename path
      exportEventlog exporter path
      shutdown exporter
      putStrLn "\nAll done."
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-to-zipkin read <program.eventlog>"
