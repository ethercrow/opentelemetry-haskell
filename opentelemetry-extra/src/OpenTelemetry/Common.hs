{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Common where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Word
import OpenTelemetry.Exporter
import OpenTelemetry.SpanContext
import System.Clock

type Timestamp = Word64

data TagValue
  = StringTagValue !T.Text
  | BoolTagValue !Bool
  | IntTagValue !Int
  | DoubleTagValue !Double
  deriving (Eq, Show)

class ToTagValue a where
  toTagValue :: a -> TagValue

instance ToTagValue String where
  toTagValue = StringTagValue . T.pack

instance ToTagValue T.Text where
  toTagValue = StringTagValue

instance ToTagValue Bool where
  toTagValue = BoolTagValue

instance ToTagValue Int where
  toTagValue = IntTagValue

data Span
  = Span
      { spanContext :: {-# UNPACK #-} !SpanContext,
        spanOperation :: T.Text,
        spanStartedAt :: !Timestamp,
        spanFinishedAt :: !Timestamp,
        spanTags :: !(HM.HashMap T.Text TagValue),
        spanEvents :: [SpanEvent],
        spanStatus :: !SpanStatus,
        spanParentId :: Maybe SpanId
      }
  deriving (Show, Eq)

spanTraceId :: Span -> TraceId
spanTraceId Span {spanContext = SpanContext _ tid} = tid

spanId :: Span -> SpanId
spanId Span {spanContext = SpanContext sid _} = sid

data SpanEvent
  = SpanEvent
      { spanEventTimestamp :: !Timestamp,
        spanEventKey :: !T.Text,
        spanEventValue :: !T.Text
      }
  deriving (Show, Eq)

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

data OpenTelemetryConfig
  = OpenTelemetryConfig
      { otcSpanExporter :: Exporter Span
      }

now64 :: IO Timestamp
now64 = do
  TimeSpec secs nsecs <- getTime Realtime
  pure $! fromIntegral secs * 1_000_000_000 + fromIntegral nsecs
