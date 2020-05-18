{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Data.Word
import qualified OpenTelemetry.Binary.Eventlog as BE
import qualified OpenTelemetry.Eventlog as E
import OpenTelemetry.SpanContext


bigId :: Word64
bigId = (maxBound `div` 2)


traceId :: TraceId
traceId = TId bigId

spanId :: SpanId
spanId = SId bigId

traceInText :: IO ()
traceInText = do
  E.withSpan "txt span" $ \spanInFlight ->
   do
     E.setTag spanInFlight "txt tag name" "txt tag value"
     E.addEvent spanInFlight "txt event name" "txt event name"
     E.setTraceId spanInFlight traceId
     E.setSpanId spanInFlight spanId

traceInBinary :: IO ()
traceInBinary = do
  BE.withSpan "bin span" $ \spanInFlight ->
   do
     BE.setTag spanInFlight "bin tag name" "bin tag value"
     BE.addEvent spanInFlight "bin event name" "bin event name"
     BE.setTraceId spanInFlight traceId
     BE.setSpanId spanInFlight spanId


-- run as follows
-- benchmark +RTS -l-au -oltrace.eventlog
main :: IO ()
main = do
  defaultMain [
        bench "in text   mode" $ whnfIO traceInText,
        bench "in binary mode" $ whnfIO traceInBinary
       ]
