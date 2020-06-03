{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Console
import Data.Aeson as A
import Data.ProtoLens (defMessage, showMessage)
import Data.ProtoLens.Encoding (encodeMessage, decodeMessageOrDie)

import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Data.Maybe
import Lens.Micro
import Lens.Micro.TH
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Exporter

import Proto.Opentelemetry.Proto.Common.V1.Common as C
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields as C
import Proto.Opentelemetry.Proto.Resource.V1.Resource as R
import Proto.Opentelemetry.Proto.Trace.V1.Trace as T
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields as T
import Resource
import System.FilePath.Posix


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
      let encodedHeader = encodeMessage . (\x -> (convertTo x) :: R.Resource) $ header
      BS.writeFile dstPath encodedHeader
      let msg :: R.Resource = decodeMessageOrDie encodedHeader
      Prelude.putStrLn . showMessage $ msg
