module OpenTelemetry.Parser (State (..)) where

import qualified Data.IntMap as IM
import qualified Data.HashMap.Strict as HM
import Data.Word
import GHC.RTS.Events (Timestamp)
import OpenTelemetry.Common (Span (..))
import OpenTelemetry.SpanContext
import qualified System.Random.SplitMix as R

type ThreadId = Word32

data State = S
  { originTimestamp :: Timestamp,
    threadMap :: IM.IntMap ThreadId,
    spans :: HM.HashMap SpanId Span,
    traceMap :: HM.HashMap ThreadId TraceId,
    serial2sid :: HM.HashMap Word64 SpanId,
    thread2sid :: HM.HashMap ThreadId SpanId,
    randomGen :: R.SMGen
  }
  deriving (Show)
