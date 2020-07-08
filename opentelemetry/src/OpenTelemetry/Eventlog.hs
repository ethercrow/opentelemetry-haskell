{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module OpenTelemetry.Eventlog
  ( beginSpan,
    endSpan,
    withSpan,
    withSpan_,
    setSpanId,
    setTraceId,
    setTag,
    addEvent,
    setParentSpanContext,
    mkCounter,
    mkUpDownCounter,
    mkValueRecorder,
    mkSumObserver,
    mkUpDownSumObserver,
    mkValueObserver,
    add,
    record,
    observe,
    SpanInFlight (..),
    module OpenTelemetry.Metrics
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import OpenTelemetry.Eventlog_Internal (SpanInFlight (..))
import qualified OpenTelemetry.Eventlog_Internal as I
import OpenTelemetry.SpanContext
import OpenTelemetry.Metrics

{-# INLINE withSpan #-}
withSpan ::
  forall m a.
  (MonadIO m, MonadMask m) =>
  BS.ByteString ->
  (SpanInFlight -> m a) ->
  m a
withSpan operation action =
  fst
    <$> generalBracket
      (liftIO $ beginSpan operation)
      ( \sp exitcase -> liftIO $ do
          case exitcase of
            ExitCaseSuccess _ -> pure ()
            ExitCaseException e -> do
              setTag sp "error" "true"
              setTag sp "error.message" (BS8.pack $ take I.maxMsgLen $ show e)
            ExitCaseAbort -> do
              setTag sp "error" "true"
              setTag sp "error.message" "abort"
          liftIO $ endSpan sp
      )
      action

{-# INLINE withSpan_ #-}
withSpan_ :: (MonadIO m, MonadMask m) => BS.ByteString -> m a -> m a
withSpan_ operation action = withSpan operation (const action)

{-# INLINE setSpanId #-}
setSpanId :: MonadIO m => SpanInFlight -> SpanId -> m ()
setSpanId sp sid = I.traceBuilder $ I.builder_setSpanId sp sid

{-# INLINE setTraceId #-}
setTraceId :: MonadIO m => SpanInFlight -> TraceId -> m ()
setTraceId sp tid = I.traceBuilder $ I.builder_setTraceId sp tid

{-# INLINE beginSpan #-}
beginSpan :: MonadIO m => BS.ByteString -> m SpanInFlight
beginSpan operation = do
  u <- I.nextLocalSpan
  I.traceBuilder $ I.builder_beginSpan u operation
  pure u

{-# INLINE endSpan #-}
endSpan :: MonadIO m => SpanInFlight -> m ()
endSpan sp = I.traceBuilder $ I.builder_endSpan sp

{-# INLINE setTag #-}
setTag :: MonadIO m => SpanInFlight -> BS.ByteString -> BS.ByteString -> m ()
setTag sp k v = I.traceBuilder $ I.builder_setTag sp k v

{-# INLINE addEvent #-}
addEvent :: MonadIO m => SpanInFlight -> BS.ByteString -> BS.ByteString -> m ()
addEvent sp k v = I.traceBuilder $ I.builder_addEvent sp k v

{-# INLINE setParentSpanContext #-}
setParentSpanContext :: MonadIO m => SpanInFlight -> SpanContext -> m ()
setParentSpanContext sp ctx = I.traceBuilder $ I.builder_setParentSpanContext sp ctx

{-# INLINE mkCounter #-}
mkCounter :: MonadIO m => InstrumentName -> m Counter
mkCounter name = do
  inst <- Counter name <$> I.nextInstrumentId
  I.traceBuilder $ I.builder_declareInstrument inst
  return inst

{-# INLINE mkUpDownCounter #-}
mkUpDownCounter :: MonadIO m => InstrumentName -> m UpDownCounter
mkUpDownCounter name = do
  inst <- UpDownCounter name <$> I.nextInstrumentId
  I.traceBuilder $ I.builder_declareInstrument inst
  return inst

{-# INLINE mkValueRecorder #-}
mkValueRecorder :: MonadIO m => InstrumentName -> m ValueRecorder
mkValueRecorder name = do
  inst <- ValueRecorder name <$> I.nextInstrumentId
  I.traceBuilder $ I.builder_declareInstrument inst
  return inst

{-# INLINE mkSumObserver #-}
mkSumObserver :: MonadIO m => InstrumentName -> m SumObserver
mkSumObserver name = do
  inst <- SumObserver name <$> I.nextInstrumentId
  I.traceBuilder $ I.builder_declareInstrument inst
  return inst

{-# INLINE mkUpDownSumObserver #-}
mkUpDownSumObserver :: MonadIO m => InstrumentName -> m UpDownSumObserver
mkUpDownSumObserver name = do
  inst <- UpDownSumObserver name <$> I.nextInstrumentId
  I.traceBuilder $ I.builder_declareInstrument inst
  return inst

{-# INLINE mkValueObserver #-}
mkValueObserver :: MonadIO m => InstrumentName -> m ValueObserver
mkValueObserver name = do
  inst <- ValueObserver name <$> I.nextInstrumentId
  I.traceBuilder $ I.builder_declareInstrument inst
  return inst

-- | Take a measurement for a synchronous, additive instrument ('Counter', 'UpDownCounter')
{-# INLINE add #-}
add :: MonadIO m => Instrument 'Synchronous 'Additive m' -> Int -> m ()
add i v = I.traceBuilder $ I.builder_captureMetric (instrumentId i) v

-- | Take a measurement for a synchronous, non-additive instrument ('ValueRecorder')
{-# INLINE record #-}
record :: MonadIO m => Instrument 'Synchronous 'NonAdditive m' -> Int -> m ()
record i v = I.traceBuilder $ I.builder_captureMetric (instrumentId i) v

-- | Take a measurement for an asynchronous instrument ('SumObserver', 'UpDownSumObserver', 'ValueObserver')
{-# INLINE observe #-}
observe :: MonadIO m => Instrument 'Asynchronous a m' -> Int -> m ()
observe i v = I.traceBuilder $ I.builder_captureMetric (instrumentId i) v
