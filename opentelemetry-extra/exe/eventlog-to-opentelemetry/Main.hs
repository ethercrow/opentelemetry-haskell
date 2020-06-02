{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.ProtoLens (defMessage, showMessage)
import Data.ProtoLens.Encoding (encodeMessage, decodeMessageOrDie)

import Data.ByteString as BS
import Data.Maybe
import Lens.Micro
import Lens.Micro.TH
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Exporter

import Options.Applicative as OA
import Proto.Opentelemetry.Proto.Common.V1.Common as C
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields as C
import Proto.Opentelemetry.Proto.Resource.V1.Resource as R
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields as R
import Proto.Opentelemetry.Proto.Trace.V1.Trace as T
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields as T

import System.FilePath.Posix


data Command = TraceCommand
    { _trCmdSrc :: FilePath
    , _trCmdDst :: Maybe FilePath
    } deriving (Show)

makeLenses ''Command

data ConsoleOptions = ConsoleOptions
    { _coCmd :: Command
    } deriving (Show)


makeLenses ''ConsoleOptions

resource1 :: C.AttributeKeyValue
resource1 =
  defMessage
      & C.key      .~ "Hello"
      & C.type'     .~ C.AttributeKeyValue'STRING
      & C.stringValue .~ "World"

main :: IO ()
main = do
  cmd <- parseConsoleOptions
  case cmd^.coCmd of
    TraceCommand {..} -> do
      let defaultDst = (-<.> "ot") . takeFileName $ cmd^.coCmd.trCmdSrc
      let dstPath = fromMaybe defaultDst (cmd^.coCmd.trCmdDst)
      let msg :: C.AttributeKeyValue = decodeMessageOrDie . encodeMessage $ resource1
      BS.writeFile dstPath . encodeMessage $ resource1
      Prelude.putStrLn . showMessage $ msg & (C.stringValue .~ "99")


extractTraceFromEventlogFileCmd :: Parser Command
extractTraceFromEventlogFileCmd
    = TraceCommand <$> argument str (metavar "SRCFILE")
                   <*> option auto
                        (  long "out"
                        <> help (  "Path to out OpenTelemetry trace file."
                                ++ "By default current directory and extension .ot" )
                        <> OA.value Nothing)

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
