{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Common where

import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Word
import System.Clock
import System.IO

newtype TraceId = TId Word64
  deriving (Show, Eq)

newtype SpanId = SId Word64
  deriving (Show, Eq)

type Timestamp = Word64

data Tracer threadId
  = Tracer
      { tracerSpanStacks :: HM.HashMap threadId (NE.NonEmpty Span)
      }

tracerPushSpan :: Tracer tid -> tid -> Span -> Tracer tid
tracerPushSpan t tid sp = t

tracerPopSpan :: Tracer tid -> tid -> Span -> Tracer tid
tracerPopSpan t tid sp = t

getCurrentActiveSpan :: (Hashable tid, Eq tid) => Tracer tid -> tid -> Maybe Span
getCurrentActiveSpan (Tracer stacks) tid =
  case HM.lookup tid stacks of
    Nothing -> Nothing
    Just (sp NE.:| _) -> Just sp

createTracer :: (Hashable tid, Eq tid) => IO (Tracer tid)
createTracer = pure $ Tracer mempty

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

emptySpan :: Span
emptySpan = Span (SpanContext (SId 0) (TId 0)) "" 0 0 OK

spanTraceId :: Span -> TraceId
spanTraceId Span {spanContext = SpanContext _ tid} = tid

spanId :: Span -> SpanId
spanId Span {spanContext = SpanContext sid _} = sid

data SpanStatus = OK
  deriving (Show, Eq)

data Event
  = Event T.Text Timestamp
  deriving (Show, Eq)

data SpanProcessor
  = SpanProcessor
      { onStart :: Span -> IO (),
        onEnd :: Span -> IO ()
      }

data ExportResult
  = ExportSuccess
  | ExportFailedRetryable
  | ExportFailedNotRetryable

data Exporter thing
  = Exporter
      { export :: [thing] -> IO ExportResult,
        shutdown :: IO ()
      }

data OpenTelemetryConfig
  = OpenTelemetryConfig
      { otcSpanExporter :: Exporter Span
      }

now64 :: IO Timestamp
now64 = do
  TimeSpec secs nsecs <- getTime Monotonic
  pure $! fromIntegral secs * 1_000_000_000 + fromIntegral nsecs
