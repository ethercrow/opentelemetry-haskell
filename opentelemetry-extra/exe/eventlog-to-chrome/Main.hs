{-# LANGUAGE OverloadedStrings #-}

module Main where

import OpenTelemetry.ChromeExporter
import Options.Applicative
import Text.Printf

data ConsoleOptions = ConsoleOptions Command deriving (Show)

data Command = EventlogToChromeCmd FilePath deriving (Show)

main :: IO ()
main = do
  (ConsoleOptions cmd) <- parseConsoleOptions
  case cmd of
    EventlogToChromeCmd path -> do
      let target_path = (path <> ".trace.json")
      printf "Converting %s to %s...\n" path target_path
      eventlogToChrome path target_path
      putStrLn "\nAll done."

readEventlogFileCmdParser :: Parser Command
readEventlogFileCmdParser
    = EventlogToChromeCmd <$> argument str (metavar "FILE")

consoleOptionParser :: Parser ConsoleOptions
consoleOptionParser
    = ConsoleOptions
      <$> hsubparser
              (command "read"
               (info readEventlogFileCmdParser
                         (progDesc "converts eventlog into chrome trace")))

parseConsoleOptions :: IO ConsoleOptions
parseConsoleOptions
    = execParser $ info (consoleOptionParser <**> helper) fullDesc
