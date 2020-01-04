module OpenTracing.API.Explicit where

import OpenTracing.API.Common
import System.Random

startRootSpan :: T.Text -> IO Span
startRootSpan name = do
  timestamp <- now64
  sid <- randomIO
  pure $! Span sid sid name timestamp 0

startChildSpanOf :: Span -> T.Text -> IO Span
startChildSpanOf parent name = do
  timestamp <- now64
  sid <- randomIO
  pure $! Span sid (spanTraceId parent) name timestamp 0

finishSpan :: Span -> IO Span
finishSpan sp = do
  timestamp <- now
  sp {spanFinishedAt = timestamp}
