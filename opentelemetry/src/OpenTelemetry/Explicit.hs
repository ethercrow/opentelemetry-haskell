module OpenTelemetry.Explicit
  ( startRootSpan,
    startChildSpan,
    endSpan,
    setTag,
    addEvent,
    createOpenTelemetryClient,
  )
where

import qualified Data.Text as T
import GHC.Conc
import OpenTelemetry.Common
import System.Random

data Client
  = Client
      { cTracer :: Tracer ThreadId,
        cSpanExporter :: Exporter Span
      }

startRootSpan :: Client -> T.Text -> IO Span
startRootSpan _tracer name = do
  timestamp <- now64
  sid <- randomIO
  pure $! Span (SpanContext (SId sid) (TId sid)) name timestamp 0 mempty mempty OK Nothing

startChildSpan :: Client -> Span -> T.Text -> IO Span
startChildSpan _tracer parent name = do
  timestamp <- now64
  sid <- randomIO
  pure $! Span (SpanContext (SId sid) (spanTraceId parent)) name timestamp 0 mempty mempty OK (Just $ spanId parent)

endSpan :: Client -> Span -> IO ()
endSpan _tracer sp = do
  timestamp <- now64
  pure $! sp {spanFinishedAt = timestamp}
  pure ()

setTag :: Monad m => Client -> Span -> T.Text -> value -> m ()
setTag _tracer _span _key _value = pure ()

addEvent :: Monad m => Client -> Span -> T.Text -> m ()
addEvent _tracer _span _name = pure ()

createOpenTelemetryClient :: Exporter Span -> IO Client
createOpenTelemetryClient e = do
  t <- createTracer
  pure $ Client t e
