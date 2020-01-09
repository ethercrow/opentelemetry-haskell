module OpenTelemetry.Common where

import qualified Data.Text as T
import Data.Word

newtype TraceId = TId Word64
  deriving (Show, Eq)

newtype SpanId = SId Word64
  deriving (Show, Eq)

type Timestamp = Word64

data Tracer
  = Tracer
      { getCurrentActiveSpan :: IO Span,
        createSpan :: T.Text -> IO Span,
        activateSpan :: Span -> IO ()
      }

data SpanContext = SpanContext !SpanId !TraceId
  deriving (Show, Eq)

data Span
  = Span
      { spanContext :: {-# UNPACK #-} !SpanContext,
        spanOperation :: T.Text,
        spanStartedAt :: !Timestamp,
        spanFinishedAt :: !Timestamp,
        spanStatus :: !SpanStatus
      }
  deriving (Show, Eq)

spanTraceId :: Span -> TraceId
spanTraceId Span {spanContext = SpanContext _ tid} = tid

spanId :: Span -> SpanId
spanId Span {spanContext = SpanContext sid _} = sid

data SpanStatus = OK
  deriving (Show, Eq)

data Event
  = Event T.Text Timestamp
  deriving (Show, Eq)
