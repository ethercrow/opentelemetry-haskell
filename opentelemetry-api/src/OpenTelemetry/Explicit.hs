module OpenTelemetry.Explicit
  ( startRootSpan,
    startChildSpan,
    endSpan,
    setTag,
    addEvent,
  )
where

import qualified Data.Text as T
import OpenTelemetry.Common
import System.Clock
import System.Random

startRootSpan :: Tracer -> T.Text -> IO Span
startRootSpan _tracer name = do
  timestamp <- now64
  sid <- randomIO
  pure $! Span (SpanContext (SId sid) (TId sid)) name timestamp 0 OK

startChildSpan :: Tracer -> Span -> T.Text -> IO Span
startChildSpan _tracer parent name = do
  timestamp <- now64
  sid <- randomIO
  pure $! Span (SpanContext (SId sid) (spanTraceId parent)) name timestamp 0 OK

endSpan :: Tracer -> Span -> IO ()
endSpan _tracer sp = do
  timestamp <- now64
  pure $! sp {spanFinishedAt = timestamp}
  pure ()

now64 :: IO Timestamp
now64 = do
  TimeSpec secs nsecs <- getTime Monotonic
  pure $! fromIntegral secs * 1_000_000_000 + fromIntegral nsecs

setTag :: Monad m => Tracer -> Span -> T.Text -> value -> m ()
setTag _tracer _span _key _value = pure ()

addEvent :: Monad m => Tracer -> Span -> T.Text -> m ()
addEvent _tracer _span _name = pure ()
