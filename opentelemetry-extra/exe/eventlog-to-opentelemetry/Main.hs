{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Data.ProtoLens (defMessage, showMessage)
import Data.ProtoLens.Encoding (encodeMessage, decodeMessageOrDie)

import Data.ByteString as BS

import Lens.Micro

import Proto.Opentelemetry.Proto.Common.V1.Common as C
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields as C
import Proto.Opentelemetry.Proto.Resource.V1.Resource as R
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields as R
import Proto.Opentelemetry.Proto.Trace.V1.Trace as T
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields as T


resource1 :: C.AttributeKeyValue
resource1 =
  defMessage
      & C.key      .~ "Hello"
      & C.type'     .~ C.AttributeKeyValue'STRING
      & C.stringValue .~ "World"

main :: IO ()
main = do
  let msg :: C.AttributeKeyValue = decodeMessageOrDie . encodeMessage $ resource1
  BS.writeFile "person.bin" . encodeMessage $ resource1
  Prelude.putStrLn . showMessage $ msg & (C.stringValue .~ "99")
