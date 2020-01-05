{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SomeUsageOfImplicitApi where

import Control.Concurrent
import OpenTelemetry.FileTracer
import OpenTelemetry.Implicit

main :: IO ()
main = do
  tracer <- mkFileTracer "helloworld.trace"
  withImplicitTracer tracer $ do
    result <- pieceOfSeriousBusinessLogic 42
    print result

pieceOfSeriousBusinessLogic :: Int -> IO Int
pieceOfSeriousBusinessLogic input = withSpan "serious business" $ do
  let result = 2 * input
  -- setTag is value-polymorphic

  setTag "input" input -- Int (inferred)
  setTag @Int "result" result -- Int (explicit)
  setTag "seriousness" "serious" -- Text (inferred)
  setTag "error" False -- Bool
  setTag "confidence" 99.99 -- Double (inferred)
  setTag @Double "profit" 99 -- Double (explicit)
  setTag @Int "largest integer below 100" 99 -- Int (inferred)

  -- TODO: JSON values

  addEvent "rpc roundtrip begin"
  withSpan "leveraging synergies" $ do
    threadDelay 10000
    addEvent "enough synergies leveraged"
  addEvent "All your base are belong to us"
  addEvent "rpc roundtrip end"
  pure result
