{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Eventlog where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS8
import Data.Unique
import Data.Word
import Debug.Trace
import OpenTelemetry.SpanContext
import Text.Printf
import Prelude hiding (span)

-- TODO(divanov): replace traceEventIO with the bytestring based equivalent

-- This is not a Span Id in terms of OpenTelemetry.
-- It's unique only in scope of one process, not globally.
type ProcessLocalSpanSerialNumber = Word64

newtype SpanInFlight = SpanInFlight ProcessLocalSpanSerialNumber

beginSpan :: MonadIO m => String -> m SpanInFlight
beginSpan operation = do
  u64 <- fromIntegral . hashUnique <$> liftIO newUnique
  liftIO $ traceEventIO (printf "ot2 begin span %d %s" u64 operation)
  pure $ SpanInFlight u64

endSpan :: MonadIO m => SpanInFlight -> m ()
endSpan (SpanInFlight u64) = liftIO $ traceEventIO (printf "ot2 end span %d" u64)

setTag :: MonadIO m => SpanInFlight -> String -> BS8.ByteString -> m ()
setTag (SpanInFlight u64) k v = liftIO $ traceEventIO (printf "ot2 set tag %d %s %s" u64 k (BS8.unpack v))

addEvent :: MonadIO m => SpanInFlight -> String -> BS8.ByteString -> m ()
addEvent (SpanInFlight u64) k v = liftIO $ traceEventIO (printf "ot2 add event %d %s %s" u64 k (BS8.unpack v))

setParentSpanContext :: MonadIO m => SpanInFlight -> SpanContext -> m ()
setParentSpanContext (SpanInFlight u64) (SpanContext (SId sid) (TId tid)) =
  liftIO $ traceEventIO (printf "ot2 set parent %d %016x %016x" u64 tid sid)

setTraceId :: MonadIO m => SpanInFlight -> TraceId -> m ()
setTraceId (SpanInFlight u64) (TId tid) =
  liftIO $ traceEventIO (printf "ot2 set traceid %d %016x" u64 tid)

setSpanId :: MonadIO m => SpanInFlight -> SpanId -> m ()
setSpanId (SpanInFlight u64) (SId sid) =
  liftIO $ traceEventIO (printf "ot2 set spanid %d %016x" u64 sid)

withSpan :: forall m a. (MonadIO m, MonadMask m) => String -> (SpanInFlight -> m a) -> m a
withSpan operation action =
  fst
    <$> generalBracket
      (liftIO $ beginSpan operation)
      ( \span exitcase -> liftIO $ do
          case exitcase of
            ExitCaseSuccess _ -> pure ()
            ExitCaseException e -> do
              setTag span "error" "true"
              setTag span "error.message" (BS8.pack $ show e)
            ExitCaseAbort -> do
              setTag span "error" "true"
              setTag span "error.message" "abort"
          liftIO $ endSpan span
      )
      action

withSpan_ :: (MonadIO m, MonadMask m) => String -> m a -> m a
withSpan_ operation action = withSpan operation (const action)
