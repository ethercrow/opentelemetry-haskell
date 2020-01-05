module OpenTelemetry.Explicit
  ( startRootSpan,
    startChildSpanOf,
    finishSpan,
  )
where

import qualified Data.Text as T
import OpenTelemetry.Common
import System.Clock
import System.Random

startRootSpan :: T.Text -> IO Span
startRootSpan name = do
  timestamp <- now64
  sid <- randomIO
  pure $! Span (SId sid) (TId sid) name timestamp 0

startChildSpanOf :: Span -> T.Text -> IO Span
startChildSpanOf parent name = do
  timestamp <- now64
  sid <- randomIO
  pure $! Span (SId sid) (spanTraceId parent) name timestamp 0

finishSpan :: Span -> IO Span
finishSpan sp = do
  timestamp <- now64
  pure $! sp {spanFinishedAt = timestamp}

now64 :: IO Timestamp
now64 = do
  TimeSpec secs nsecs <- getTime Monotonic
  pure $! fromIntegral secs * 1_000_000_000 + fromIntegral nsecs
