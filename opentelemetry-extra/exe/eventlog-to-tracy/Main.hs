module Main where

import Data.List (intercalate)
import OpenTelemetry.ChromeExporter
import System.Directory (findExecutable)
import System.Environment (getArgs)
import System.Exit
import System.Process

help :: IO ()
help = do
  putStrLn "Converts eventlog to Tracy format and launches the viewer."
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  eventlog-to-tracy [--collapse-threads | --split-threads] <application.eventlog>"
  exitFailure

tracyInstallInstructions :: String
tracyInstallInstructions =
  intercalate
    ", "
    [ "https://github.com/wolfpld/tracy from source",
      "or get it from AUR or nixpkgs"
    ]

callProcessOrExplain :: FilePath -> String -> [String] -> IO ()
callProcessOrExplain filename instruction args = do
  exists <- findExecutable filename
  case exists of
    Just _ -> callProcess filename args
    Nothing -> do
      putStrLn $ filename <> ": command not found, please install " <> instruction
      exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"] -> help
    ["--help"] -> help
    ["help"] -> help
    [eventlogFile] -> work eventlogFile SplitThreads
    ["--collapse-threads", eventlogFile] -> work eventlogFile CollapseThreads
    ["--split-threads", eventlogFile] -> work eventlogFile SplitThreads
    _ -> help

work :: FilePath -> ThreadPresentation -> IO ()
work inputFile doWeCollapseThreads = do
  let chromeFile = inputFile ++ ".trace.json"
      tracyFile = inputFile ++ ".tracy"
  putStrLn $ "Converting " <> inputFile <> " to " <> chromeFile <> "..."
  eventlogToChrome inputFile chromeFile doWeCollapseThreads
  callProcessOrExplain "import-chrome" tracyInstallInstructions [chromeFile, tracyFile]
  callProcessOrExplain "Tracy" tracyInstallInstructions [tracyFile]
