module Main where

import System.Environment (getArgs)
import System.Exit
import System.Process

help :: IO ()
help = do
  putStrLn "Converts eventlog to Tracy format and launches the viewer."
  putStrLn "Path to eventlog is expected."
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"]  -> help
    ["--help"] -> help
    [eventlogFile] -> do
      let chromeFile = eventlogFile ++ ".trace.json"
      let tracyFile = eventlogFile ++ ".tracy"
      callProcess "eventlog-to-chrome" ["read", eventlogFile]
      callProcess "import-chrome" [chromeFile, tracyFile]
      callProcess "Tracy" [tracyFile]
    _ -> help
