module Main where

import OpenTelemetry.ChromeExporter
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Exporter
import System.Clock
import System.Environment (getArgs)
import System.Exit
import System.Process
import System.Directory

help :: IO ()
help = do
  putStrLn "Converts eventlog to Tracy format and launches the viewer."
  putStrLn "Path to eventlog is expected."
  exitFailure

callProcessOrExplain :: FilePath -> String -> [String] -> IO ()
callProcessOrExplain filename instruction args = do
  exists <- findExecutable filename
  case exists of
    Just _ -> callProcess filename args
    Nothing -> do
      putStrLn $ filename <> ": command not found, please install " <> instruction
      exitFailure

eventlogToChrome :: FilePath -> FilePath -> IO ()
eventlogToChrome eventlogFile chromeFile = do
  putStrLn $ "Converting " <> eventlogFile <> " to " <> chromeFile <> "..."
  exporter <- createChromeSpanExporter chromeFile
  origin_timestamp <- fromIntegral . toNanoSecs <$> getTime Realtime
  work origin_timestamp exporter $ EventLogFilename eventlogFile
  shutdown exporter

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"]  -> help
    ["--help"] -> help
    [eventlogFile] -> do
      let chromeFile = eventlogFile ++ ".trace.json"
      let tracyFile = eventlogFile ++ ".tracy"
      eventlogToChrome eventlogFile chromeFile
      callProcessOrExplain "import-chrome" "chrome" [chromeFile, tracyFile]
      callProcessOrExplain "Tracy" "Tracy" [tracyFile]
    _ -> help
