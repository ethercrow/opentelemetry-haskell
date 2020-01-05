{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SomeUsageOfImplicitApi where

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

  pure result
