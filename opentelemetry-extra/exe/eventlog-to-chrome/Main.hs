{-# LANGUAGE OverloadedStrings #-}

module Main where

import OpenTelemetry.ChromeExporter
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Exporter
import Options.Applicative
import System.Clock
import System.IO
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
      exporter <- createChromeSpanExporter target_path
      origin_timestamp <- fromIntegral . toNanoSecs <$> getTime Realtime
      withFile path ReadMode (work StopOnEOF origin_timestamp exporter)
      shutdown exporter
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
