{-# LANGUAGE OverloadedStrings #-}

module Main where

import OpenTelemetry.ChromeExporter
import System.Environment
import Text.Printf

data ConsoleOptions = ConsoleOptions Command deriving (Show)

data Command = EventlogToChromeCmd FilePath deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["read", path] -> do
      let target_path = (path <> ".trace.json")
      printf "Converting %s to %s...\n" path target_path
      eventlogToChrome path target_path
      putStrLn "\nAll done."
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-to-chrome read <program.eventlog>"
