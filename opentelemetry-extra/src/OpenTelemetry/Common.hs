{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OpenTelemetry.Common where

import Data.Aeson
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Word
import GHC.Generics
import OpenTelemetry.Exporter
import OpenTelemetry.SpanContext
import System.Clock
import Data.String
import OpenTelemetry.Metrics (SomeInstrument)

type Timestamp = Word64

newtype SpanName = SpanName T.Text deriving (Show, Eq, Generic)
newtype TagName = TagName T.Text deriving (Show, Eq, Generic, ToJSONKey, Hashable)
newtype TagVal = TagVal T.Text deriving (Show, Eq, Generic, ToJSON)
newtype EventName = EventName T.Text deriving (Show, Eq, Generic)
newtype EventVal = EventVal T.Text deriving (Show, Eq, Generic, ToJSON)


instance IsString TagName where
  fromString = TagName . T.pack

data TagValue
  = StringTagValue !TagVal
  | BoolTagValue !Bool
  | IntTagValue !Int
  | DoubleTagValue !Double
  deriving (Eq, Show)

class ToTagValue a where
  toTagValue :: a -> TagValue

instance ToTagValue String where
  toTagValue = StringTagValue . TagVal . T.pack

instance ToTagValue TagVal where
  toTagValue = StringTagValue

instance ToTagValue T.Text where
  toTagValue = StringTagValue . TagVal

instance ToTagValue Bool where
  toTagValue = BoolTagValue

instance ToTagValue Int where
  toTagValue = IntTagValue

data Span = Span
  { spanContext :: {-# UNPACK #-} !SpanContext,
    spanOperation :: T.Text,
    spanThreadId :: Word32,
    spanStartedAt :: !Timestamp,
    spanFinishedAt :: !Timestamp,
    spanTags :: !(HM.HashMap TagName TagValue),
    spanEvents :: [SpanEvent],
    spanStatus :: !SpanStatus,
    spanParentId :: Maybe SpanId,
    spanNanosecondsSpentInGC :: !Word64
  }
  deriving (Show, Eq)

-- | Based on https://github.com/open-telemetry/opentelemetry-proto/blob/1a931b4b57c34e7fd8f7dddcaa9b7587840e9c08/opentelemetry/proto/metrics/v1/metrics.proto#L96-L107
data Metric = Metric
  { instrument :: !SomeInstrument,
    datapoints :: ![MetricDatapoint]
  }
  deriving (Show, Eq)
data MetricDatapoint = MetricDatapoint !Timestamp !Int
  deriving (Show, Eq)

spanTraceId :: Span -> TraceId
spanTraceId Span {spanContext = SpanContext _ tid} = tid

spanId :: Span -> SpanId
spanId Span {spanContext = SpanContext sid _} = sid

data SpanEvent = SpanEvent
  { spanEventTimestamp :: !Timestamp,
    spanEventKey :: !EventName,
    spanEventValue :: !EventVal
  }
  deriving (Show, Eq)

data SpanStatus = OK
  deriving (Show, Eq)

data Event
  = Event T.Text Timestamp
  deriving (Show, Eq)

data SpanProcessor = SpanProcessor
  { onStart :: Span -> IO (),
    onEnd :: Span -> IO ()
  }

data OpenTelemetryConfig = OpenTelemetryConfig
  { otcSpanExporter :: Exporter Span
  }

now64 :: IO Timestamp
now64 = do
  TimeSpec secs nsecs <- getTime Realtime
  pure $! fromIntegral secs * 1_000_000_000 + fromIntegral nsecs

