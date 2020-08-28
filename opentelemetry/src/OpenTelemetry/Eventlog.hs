{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module OpenTelemetry.Eventlog
  ( -- * Spans
    beginSpan,
    endSpan,
    withSpan,
    withSpan_,
    setSpanId,
    setTraceId,
    setTag,
    addEvent,
    setParentSpanContext,
    SpanInFlight (..),

    -- * Metrics
    mkCounter,
    mkUpDownCounter,
    mkValueRecorder,
    mkSumObserver,
    mkUpDownSumObserver,
    mkValueObserver,
    add,
    record,
    observe,
    MI.Instrument,
    MI.SomeInstrument (..),
    MI.Counter,
    MI.UpDownCounter,
    MI.ValueRecorder,
    MI.SumObserver,
    MI.UpDownSumObserver,
    MI.ValueObserver,
    MI.Synchronicity (..),
    MI.Additivity (..),
    MI.Monotonicity (..),
    MI.InstrumentName,
    MI.InstrumentId,
    MI.instrumentName,
    MI.instrumentId,
  )
where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import OpenTelemetry.Eventlog_Internal (SpanInFlight (..))
import qualified OpenTelemetry.Eventlog_Internal as I
import qualified OpenTelemetry.Metrics_Internal as MI
import OpenTelemetry.SpanContext
import Prelude hiding (span)

#if __GLASGOW_HASKELL__ < 808

import Data.Unique
import Debug.Trace
import OpenTelemetry.Metrics_Internal

beginSpan :: MonadIO m => String -> m SpanInFlight
beginSpan operation = do
  u64 <- fromIntegral . hashUnique <$> liftIO newUnique
  liftIO $ traceEventIO (I.beginSpan' (SpanInFlight u64) operation)
  pure $ SpanInFlight u64

endSpan :: MonadIO m => SpanInFlight -> m ()
endSpan = liftIO . traceEventIO . I.endSpan'

setTag :: MonadIO m => SpanInFlight -> String -> BS.ByteString -> m ()
setTag sp k v = liftIO . traceEventIO $ I.setTag' sp k v

addEvent :: MonadIO m => SpanInFlight -> String -> BS.ByteString -> m ()
addEvent sp k v = liftIO . traceEventIO $ I.addEvent' sp k v

setParentSpanContext :: MonadIO m => SpanInFlight -> SpanContext -> m ()
setParentSpanContext sp ctx = liftIO . traceEventIO $ I.setParentSpanContext' sp ctx

setTraceId :: MonadIO m => SpanInFlight -> TraceId -> m ()
setTraceId sp tid = liftIO . traceEventIO $ I.setTraceId' sp tid

setSpanId :: MonadIO m => SpanInFlight -> SpanId -> m ()
setSpanId sp sid = liftIO . traceEventIO $ I.setSpanId' sp sid

createInstrument :: MonadIO io => MI.Instrument s a m -> io ()
createInstrument = liftIO . traceEventIO . I.createInstrument'

writeMetric :: MonadIO io => MI.Instrument s a m -> Int -> io ()
writeMetric i v = liftIO $ traceEventIO $ I.writeMetric' (instrumentId i) v

mkCounter :: MonadIO m => MI.InstrumentName -> m MI.Counter
mkCounter name = do
  inst <- MI.Counter name <$> I.nextInstrumentId
  createInstrument inst
  return inst

mkUpDownCounter :: MonadIO m => MI.InstrumentName -> m MI.UpDownCounter
mkUpDownCounter name = do
  inst <- MI.UpDownCounter name <$> I.nextInstrumentId
  createInstrument inst
  return inst

mkValueRecorder :: MonadIO m => MI.InstrumentName -> m MI.ValueRecorder
mkValueRecorder name = do
  inst <- MI.ValueRecorder name <$> I.nextInstrumentId
  createInstrument inst
  return inst

mkSumObserver :: MonadIO m => MI.InstrumentName -> m MI.SumObserver
mkSumObserver name = do
  inst <- MI.SumObserver name <$> I.nextInstrumentId
  createInstrument inst
  return inst

mkUpDownSumObserver :: MonadIO m => MI.InstrumentName -> m MI.UpDownSumObserver
mkUpDownSumObserver name = do
  inst <- MI.UpDownSumObserver name <$> I.nextInstrumentId
  createInstrument inst
  return inst

mkValueObserver :: MonadIO m => MI.InstrumentName -> m MI.ValueObserver
mkValueObserver name = do
  inst <- MI.ValueObserver name <$> I.nextInstrumentId
  createInstrument inst
  return inst

-- | Take a measurement for a synchronous, additive instrument ('Counter', 'UpDowncounter')
add :: MonadIO io => MI.Instrument 'MI.Synchronous 'MI.Additive m' -> Int -> io ()
add = writeMetric

-- | Take a measurement for a synchronous, non-additive instrument ('ValueRecorder')
record :: MonadIO io => MI.Instrument 'MI.Synchronous 'MI.NonAdditive m' -> Int -> io ()
record = writeMetric

-- | Take a measurement for an asynchronous instrument ('SumObserver', 'UpDownSumObserver', 'ValueObserver')
observe :: MonadIO io => MI.Instrument 'MI.Asynchronous a m' -> Int -> io ()
observe = writeMetric

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

#else

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
mkCounter :: MonadIO m => MI.InstrumentName -> m MI.Counter
mkCounter name = do
  inst <- MI.Counter name <$> I.nextInstrumentId
  I.traceBuilder $ I.builder_declareInstrument inst
  return inst

{-# INLINE mkUpDownCounter #-}
mkUpDownCounter :: MonadIO m => MI.InstrumentName -> m MI.UpDownCounter
mkUpDownCounter name = do
  inst <- MI.UpDownCounter name <$> I.nextInstrumentId
  I.traceBuilder $ I.builder_declareInstrument inst
  return inst

{-# INLINE mkValueRecorder #-}
mkValueRecorder :: MonadIO m => MI.InstrumentName -> m MI.ValueRecorder
mkValueRecorder name = do
  inst <- MI.ValueRecorder name <$> I.nextInstrumentId
  I.traceBuilder $ I.builder_declareInstrument inst
  return inst

{-# INLINE mkSumObserver #-}
mkSumObserver :: MonadIO m => MI.InstrumentName -> m MI.SumObserver
mkSumObserver name = do
  inst <- MI.SumObserver name <$> I.nextInstrumentId
  I.traceBuilder $ I.builder_declareInstrument inst
  return inst

{-# INLINE mkUpDownSumObserver #-}
mkUpDownSumObserver :: MonadIO m => MI.InstrumentName -> m MI.UpDownSumObserver
mkUpDownSumObserver name = do
  inst <- MI.UpDownSumObserver name <$> I.nextInstrumentId
  I.traceBuilder $ I.builder_declareInstrument inst
  return inst

{-# INLINE mkValueObserver #-}
mkValueObserver :: MonadIO m => MI.InstrumentName -> m MI.ValueObserver
mkValueObserver name = do
  inst <- MI.ValueObserver name <$> I.nextInstrumentId
  I.traceBuilder $ I.builder_declareInstrument inst
  return inst

-- | Take a measurement for a synchronous, additive instrument ('Counter', 'UpDownCounter')
{-# INLINE add #-}
add :: MonadIO m => MI.Instrument 'MI.Synchronous 'MI.Additive m' -> Int -> m ()
add i v = I.traceBuilder $ I.builder_captureMetric (MI.instrumentId i) v

-- | Take a measurement for a synchronous, non-additive instrument ('ValueRecorder')
{-# INLINE record #-}
record :: MonadIO m => MI.Instrument 'MI.Synchronous 'MI.NonAdditive m' -> Int -> m ()
record i v = I.traceBuilder $ I.builder_captureMetric (MI.instrumentId i) v

-- | Take a measurement for an asynchronous instrument ('SumObserver', 'UpDownSumObserver', 'ValueObserver')
{-# INLINE observe #-}
observe :: MonadIO m => MI.Instrument 'MI.Asynchronous a m' -> Int -> m ()
observe i v = I.traceBuilder $ I.builder_captureMetric (MI.instrumentId i) v

#endif
