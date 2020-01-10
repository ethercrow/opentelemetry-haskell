{-# LANGUAGE OverloadedStrings #-}

module SomeUsageOfExplicitApi where

import Control.Concurrent
import OpenTelemetry.Explicit
import OpenTelemetry.FileExporter

main :: IO ()
main = do
  exporter <- createFileSpanExporter "helloworld.trace"
  ot <- createOpenTelemetryClient exporter
  --
  mainSpan <- startRootSpan ot "main"
  threadDelay 10000
  warmUpSpan <- startChildSpan ot mainSpan "warm up the cache"
  threadDelay 5000
  addEvent ot warmUpSpan "still cold"
  threadDelay 10000
  addEvent ot warmUpSpan "warm now"
  setTag ot warmUpSpan "size" 9001
  endSpan ot warmUpSpan
  dataScienceSpan <- startChildSpan ot mainSpan "perform data science"
  threadDelay 100000
  setTag ot dataScienceSpan "reviews" 0
  endSpan ot dataScienceSpan
  threadDelay 10000
  endSpan ot mainSpan
