{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Common where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (sortOn)
import Data.String
import qualified Data.Text as T
import Data.Word
import GHC.Generics
import GHC.Int (Int8)
import OpenTelemetry.SpanContext
import System.Clock

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

-- | Reflects the constructors of 'OpenTelemetry.Metrics_Internal.Instrument'
data InstrumentType
  = CounterType
  | UpDownCounterType
  | ValueRecorderType
  | SumObserverType
  | UpDownSumObserverType
  | ValueObserverType
  deriving (Show, Eq, Enum, Generic)

instance Hashable InstrumentType

data CaptureInstrument = CaptureInstrument
  { instrumentType :: !InstrumentType,
    instrumentName :: !BS.ByteString
  }
  deriving (Show, Eq, Generic)

instance Hashable CaptureInstrument

-- | Based on https://github.com/open-telemetry/opentelemetry-proto/blob/1a931b4b57c34e7fd8f7dddcaa9b7587840e9c08/opentelemetry/proto/metrics/v1/metrics.proto#L96-L107
data Metric = Metric
  { instrument :: !CaptureInstrument,
    datapoints :: ![MetricDatapoint Int]
  }
  deriving (Show, Eq)

data AggregatedMetric = AggregatedMetric
  { instrument :: !CaptureInstrument,
    datapoint :: !(MetricDatapoint Int)
  }
  deriving (Show, Eq)

data MetricDatapoint a = MetricDatapoint
  { timestamp :: !Timestamp,
    value :: !a
  }
  deriving (Show, Eq, Functor)

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

data ExportResult
  = ExportSuccess
  | ExportFailedRetryable
  | ExportFailedNotRetryable
  deriving (Show, Eq)

data Exporter thing = Exporter
  { export :: [thing] -> IO ExportResult,
    shutdown :: IO ()
  }

readInstrumentTag :: Int8 -> Maybe InstrumentType
readInstrumentTag 1 = Just CounterType
readInstrumentTag 2 = Just UpDownCounterType
readInstrumentTag 3 = Just ValueRecorderType
readInstrumentTag 4 = Just SumObserverType
readInstrumentTag 5 = Just UpDownSumObserverType
readInstrumentTag 6 = Just ValueObserverType
readInstrumentTag _ = Nothing

additive :: InstrumentType -> Bool
additive CounterType = True
additive UpDownCounterType = True
additive ValueRecorderType = False
additive SumObserverType = True
additive UpDownSumObserverType = True
additive ValueObserverType = False

noopExporter :: Exporter whatever
noopExporter = Exporter (const (pure ExportFailedNotRetryable)) (pure ())

aggregated :: Exporter AggregatedMetric -> IO (Exporter Metric)
aggregated (Exporter export shutdown) = do
  -- We keep a mutable map of latest metric values. When a new datapoint comes
  -- in, it either replaces or gets added to the current value, based on whether
  -- the instrument is additive.
  currentValuesRef <- newIORef HM.empty
  return $
    Exporter
      { export = \metrics -> do
          forM_ metrics $ \(Metric instrument datapoints) -> do
            forM_ (sortOn timestamp datapoints) $ \dp@(MetricDatapoint ts value) ->
              modifyIORef currentValuesRef $
                if additive (instrumentType instrument)
                  then
                    HM.alter
                      ( \case
                          Nothing -> Just dp
                          Just (MetricDatapoint _ oldValue) -> Just (MetricDatapoint ts $ oldValue + value)
                      )
                      instrument
                  else HM.insert instrument dp

          -- Read the latest value for each instrument
          currentValues <- readIORef currentValuesRef
          export [AggregatedMetric i (currentValues HM.! i) | Metric i _ <- metrics],
        shutdown
      }

now64 :: IO Timestamp
now64 = do
  TimeSpec secs nsecs <- getTime Realtime
  pure $! fromIntegral secs * 1_000_000_000 + fromIntegral nsecs
