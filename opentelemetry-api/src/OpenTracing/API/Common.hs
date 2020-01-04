module OpenTracing.API.Common where

import Data.Int
import qualified Data.Text as T

newtype TraceId = TId Int64

newtype SpanId = SId Int64

type Timestamp = Int64

data Span
  = Span
      { spanId :: !SpanId,
        spanTraceId :: !TraceId,
        spanOperation :: T.Text,
        spanStartedAt :: !Timestamp,
        spanFinishedAt :: !Timestamp
      }
