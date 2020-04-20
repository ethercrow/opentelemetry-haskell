{-# LANGUAGE OverloadedStrings #-}

module Main where

import OpenTelemetry.ChromeExporter
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Exporter
import System.Clock
import System.Environment (getArgs)
import System.IO
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["read", path] -> do
      let target_path = (path <> ".trace.json")
      printf "Converting %s to %s...\n" path target_path
      exporter <- createChromeSpanExporter target_path
      origin_timestamp <- fromIntegral . toNanoSecs <$> getTime Realtime
      withFile path ReadMode (work origin_timestamp exporter)
      shutdown exporter
      putStrLn "\nAll done."
