{-# LANGUAGE DeriveGeneric #-}

module OpenTelemetry.SpanContext where

import Data.Hashable
import Data.Word
import GHC.Generics
import Text.Printf

newtype TraceId = TId Word64
  deriving (Eq, Ord, Generic)

instance Show TraceId where
  show (TId tid) = printf "(TId 0x%x)" tid

newtype SpanId = SId Word64
  deriving (Eq, Ord, Generic)

instance Hashable SpanId


instance Show SpanId where
  show (SId sid) = printf "(SId 0x%x)" sid

data SpanContext = SpanContext !SpanId !TraceId
  deriving (Show, Eq, Ord, Generic)
