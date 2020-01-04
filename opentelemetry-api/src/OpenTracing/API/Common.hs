module OpenTracing.API.Common where

import qualified Data.Text as T
import Data.Word

newtype TraceId = TId Word64

newtype SpanId = SId Word64

type Timestamp = Word64

data Span
  = Span
      { spanId :: !SpanId,
        spanTraceId :: !TraceId,
        spanOperation :: T.Text,
        spanStartedAt :: !Timestamp,
        spanFinishedAt :: !Timestamp
      }
