module Main where

import System.Mem
import Control.Concurrent.Async
import Control.Concurrent
import OpenTelemetry.Eventlog

main :: IO ()
main = withSpan "main" $ do
  --withSpan "very_short" $ pure ()
  --withSpan "very_short" $ pure ()
  --withSpan "very_short" $ pure ()
  --withSpan "very_short" $ pure ()
  --withSpan "very_short" $ pure ()
  ----
  --withSpan "very_short_with_tags" $ do
  --  setTag "short" "true"
  --  addEvent "message" "sup"
  --  setTag "long" "false"
  --  addEvent "message" "sup"
  --withSpan "very_short_with_tags" $ do
  --  setTag "short" "true"
  --  addEvent "message" "sup"
  --  setTag "long" "false"
  --  addEvent "message" "sup"
  --withSpan "very_short_with_tags" $ do
  --  setTag "short" "true"
  --  addEvent "message" "sup"
  --  setTag "long" "false"
  --  addEvent "message" "sup"
  ----
  --withSpan "fail" $ do
  --  setTag "error" "true"
  --  setTag "error.message" "Success avoided successfully"
  --
  bg1 <- async $ withSpan "bg_worker_10_ms" $ do
    withSpan "trigger_gc" performGC
    threadDelay 10000
  
  bg2 <- async $ withSpan "bg_worker_42_ms" $ do
     threadDelay 42000
  
  wait bg1
  wait bg2
  addEvent "message" "done"
