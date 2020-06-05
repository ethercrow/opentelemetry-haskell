module Main where

import Data.List (intercalate)
import OpenTelemetry.ChromeExporter (eventlogToChrome)
import System.Directory (findExecutable)
import System.Environment (getArgs)
import System.Exit
import System.Process

help :: IO ()
help = do
  putStrLn "Converts eventlog to Tracy format and launches the viewer."
  putStrLn "Path to eventlog is expected."
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
    [eventlogFile] -> do
      let chromeFile = eventlogFile ++ ".trace.json"
      let tracyFile = eventlogFile ++ ".tracy"
      putStrLn $ "Converting " <> eventlogFile <> " to " <> chromeFile <> "..."
      eventlogToChrome eventlogFile chromeFile
      callProcessOrExplain "import-chrome" tracyInstallInstructions [chromeFile, tracyFile]
      callProcessOrExplain "Tracy" tracyInstallInstructions [tracyFile]
    _ -> help
