{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson as A
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Encoding (encodeMessage)

import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Data.Maybe
import Lens.Micro

import OpenTelemetry.Common as OC
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Exporter

import qualified Proto.Opentelemetry.Proto.Resource.V1.Resource as R
import qualified Proto.Opentelemetry.Proto.Trace.V1.Trace as T
import qualified Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields as T
import OpenTelemetry.Console
import OpenTelemetry.Resource
import OpenTelemetry.Spans
import System.Clock
import System.FilePath.Posix
import System.IO

createExporter :: R.Resource -> FilePath -> IO (Exporter OC.Span)
createExporter resource path = do
  f <- openFile path WriteMode
  pure
    $! Exporter
      ( \sps -> do
          let msg :: T.ResourceSpans = defMessage
                    & T.resource .~ resource
                    & T.instrumentationLibrarySpans .~ [spansToLibSpans sps]
          BS.hPut f (encodeMessage msg)
          pure ExportSuccess
      )
      (hClose f)

main :: IO ()
main = do
  cmd <- parseConsoleOptions
  case cmd^.coCmd of
    TraceCommand {..} -> do
      let defaultDst = (-<.> "ot") . takeFileName $ cmd^.coCmd.trCmdSrc
      let dstPath = fromMaybe defaultDst (cmd^.coCmd.trCmdDst)
      let emptyHeaderErr = error $ "empty json header file: "
                           ++ cmd^.coCmd.trJsonHeader
      header :: ResourceHeader <- (fromMaybe emptyHeaderErr . A.decode)
                <$> LBS.readFile (cmd^.coCmd.trJsonHeader)
      exporter <- createExporter (convertTo header) dstPath
      origin_timestamp <- fromIntegral . toNanoSecs <$> getTime Realtime
      work origin_timestamp exporter $ EventLogFilename dstPath
      shutdown exporter
