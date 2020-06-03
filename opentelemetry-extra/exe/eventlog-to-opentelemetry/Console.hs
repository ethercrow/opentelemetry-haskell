{-# LANGUAGE TemplateHaskell #-}

module Console
    ( parseConsoleOptions
    , Command (..)
    , ConsoleOptions (..)
    , trCmdDst
    , trCmdSrc
    , coCmd
    , trJsonHeader
    ) where


import Lens.Micro.TH

import Options.Applicative as OA

data Command = TraceCommand
    { _trCmdSrc :: FilePath
    , _trCmdDst :: Maybe FilePath
    , _trJsonHeader :: FilePath
    } deriving (Show)

makeLenses ''Command

data ConsoleOptions = ConsoleOptions
    { _coCmd :: Command
    } deriving (Show)

makeLenses ''ConsoleOptions

extractTraceFromEventlogFileCmd :: Parser Command
extractTraceFromEventlogFileCmd
    = TraceCommand <$> argument str (metavar "SRCFILE")
                   <*> optional (strOption
                        (  long "out"
                        <> help (  "Path to out OpenTelemetry trace file."
                                ++ " By default current directory and extension .ot" )
                        ))
                   <*> option auto
                        (  long "header"
                        <> help "Path to json file with OpenTelemetry header info."
                        <> showDefault
                        <> OA.value "ot-header.json")

consoleOptionParser :: Parser ConsoleOptions
consoleOptionParser
    = ConsoleOptions
      <$> hsubparser
              (command "trace"
               (info extractTraceFromEventlogFileCmd
                         (progDesc "converts Eventlog tracing into OpenTelemtry one")))

parseConsoleOptions :: IO ConsoleOptions
parseConsoleOptions
    = execParser $ info (consoleOptionParser <**> helper) fullDesc
