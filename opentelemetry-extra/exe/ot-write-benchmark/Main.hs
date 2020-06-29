{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Word
import Gauge.Main
import qualified OpenTelemetry.Eventlog as BE
import OpenTelemetry.SpanContext

bigId :: Word64
bigId = (maxBound `div` 2)

traceId :: TraceId
traceId = TId bigId

spanId :: SpanId
spanId = SId bigId

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
  defaultMain
    [ bench "in binary mode" $ whnfIO traceInBinary
    ]
