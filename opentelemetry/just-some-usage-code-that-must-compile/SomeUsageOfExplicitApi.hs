{-# LANGUAGE OverloadedStrings #-}

module SomeUsageOfExplicitApi where

import Control.Concurrent
import OpenTelemetry.Explicit
import OpenTelemetry.FileTracer

main :: IO ()
main = do
  tracer <- mkFileTracer "helloworld.trace"
  mainSpan <- startRootSpan tracer "main"
  threadDelay 10000
  warmUpSpan <- startChildSpan tracer mainSpan "warm up the cache"
  threadDelay 5000
  addEvent tracer warmUpSpan "still cold"
  threadDelay 10000
  addEvent tracer warmUpSpan "warm now"
  setTag tracer warmUpSpan "size" 9001
  endSpan tracer warmUpSpan
  dataScienceSpan <- startChildSpan tracer mainSpan "perform data science"
  threadDelay 100000
  setTag tracer dataScienceSpan "reviews" 0
  endSpan tracer dataScienceSpan
  threadDelay 10000
  endSpan tracer mainSpan
