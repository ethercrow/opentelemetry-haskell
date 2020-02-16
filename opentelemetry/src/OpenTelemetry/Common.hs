{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Common where

import qualified Data.HashMap.Strict as HM
import Data.Hashable
import GHC.Generics
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.Text as T
import Data.Word
import System.Clock
import Text.Printf

newtype TraceId = TId Word64
  deriving (Eq, Generic)
  deriving (Hashable) via Word64

instance Show TraceId where
  show (TId tid) = printf "TraceId %x" tid

newtype SpanId = SId Word64
  deriving (Eq, Generic)
  deriving (Hashable) via Word64

instance Show SpanId where
  show (SId sid) = printf "SpanId %x" sid

type Timestamp = Word64

data Tracer threadId
  = Tracer
      { tracerSpanStacks :: !(HM.HashMap threadId (NE.NonEmpty Span))
      }
      deriving (Eq, Show)

tracerPushSpan :: (Eq tid, Hashable tid) => Tracer tid -> tid -> Span -> Tracer tid
tracerPushSpan t@(Tracer {..}) tid sp =
  case HM.lookup tid tracerSpanStacks of
    Nothing ->
      let !stacks = HM.insert tid (sp :| []) tracerSpanStacks
      in Tracer stacks
    Just sps ->
      let !stacks = HM.insert tid (sp <| sps) tracerSpanStacks
      in t { tracerSpanStacks = stacks }

tracerPopSpan :: (Eq tid, Hashable tid) => Tracer tid -> tid -> (Maybe Span, Tracer tid)
tracerPopSpan t@(Tracer {..}) tid =
  case HM.lookup tid tracerSpanStacks of
    Nothing -> (Nothing, t)
    Just (sp :| sps) ->
      let stacks =
            case NE.nonEmpty sps of
              Nothing -> HM.delete tid tracerSpanStacks
              Just sps' -> HM.insert tid sps' tracerSpanStacks
       in (Just sp, Tracer stacks)

tracerGetCurrentActiveSpan :: (Hashable tid, Eq tid) => Tracer tid -> tid -> Maybe Span
tracerGetCurrentActiveSpan (Tracer stacks) tid =
  case HM.lookup tid stacks of
    Nothing -> Nothing
    Just (sp NE.:| _) -> Just sp

createTracer :: (Hashable tid, Eq tid) => IO (Tracer tid)
createTracer = pure $ Tracer mempty

data SpanContext = SpanContext !SpanId !TraceId
  deriving (Show, Eq, Generic)

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

data Span
  = Span
      { spanContext :: {-# UNPACK #-} !SpanContext,
        spanOperation :: T.Text,
        spanStartedAt :: !Timestamp,
        spanFinishedAt :: !Timestamp,
        spanTags :: !(HM.HashMap T.Text TagValue),
        spanStatus :: !SpanStatus,
        spanParentId :: Maybe SpanId
      }
  deriving (Show, Eq)

emptySpan :: Span
emptySpan = Span (SpanContext (SId 0) (TId 0)) "" 0 0 mempty OK Nothing

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
  deriving (Show, Eq)

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
