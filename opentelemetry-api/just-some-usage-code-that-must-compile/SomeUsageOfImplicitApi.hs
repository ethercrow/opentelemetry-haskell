{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SomeUsageOfImplicitApi where

import Control.Concurrent
import OpenTelemetry.Implicit

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

  -- addLog is value-polymorphic

  addLog "rpc roundtrip begin" 8999 -- Int (inferred)
  withSpan "leveraging synergies" $ do
    threadDelay 10000
  addLog "message" "All your base are belong to us" -- Text (inferred)
  addLog "rpc roundtrip end" 9001 -- Int (inferred)
  pure result
