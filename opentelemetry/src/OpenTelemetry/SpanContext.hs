{-# LANGUAGE DeriveGeneric #-}

module OpenTelemetry.SpanContext where

import Data.Word
import GHC.Generics
import Text.Printf

newtype TraceId = TId Word64
  deriving (Eq, Generic)

instance Show TraceId where
  show (TId tid) = printf "(TraceId 0x%x)" tid

newtype SpanId = SId Word64
  deriving (Eq, Generic)

instance Show SpanId where
  show (SId sid) = printf "(SpanId 0x%x)" sid

data SpanContext = SpanContext !SpanId !TraceId
  deriving (Show, Eq, Generic)
