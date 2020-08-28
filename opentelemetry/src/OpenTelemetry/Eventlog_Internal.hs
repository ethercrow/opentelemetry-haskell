{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module OpenTelemetry.Eventlog_Internal where

import Control.Monad.IO.Class
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Hashable
#if __GLASGOW_HASKELL__ < 808
import Debug.Trace.ByteString
#else
import Debug.Trace.Binary
#endif

import Data.Int
import Data.Unique
import Data.Word (Word64, Word8)
import OpenTelemetry.Metrics_Internal as MI
import OpenTelemetry.SpanContext
import Text.Printf
import Prelude hiding (span)

-- This is not a Span Id in terms of OpenTelemetry.
-- It's unique only in scope of one process, not globally.
type ProcessLocalSpanSerialNumber = Word64

newtype SpanInFlight = SpanInFlight ProcessLocalSpanSerialNumber
  deriving (Show, Eq, Hashable)

newtype MsgType = MsgType Word8
  deriving (Show)

pattern BEGIN_SPAN, END_SPAN, TAG, EVENT, SET_PARENT_CONTEXT, SET_TRACE_ID, SET_SPAN_ID, DECLARE_INSTRUMENT, METRIC_CAPTURE :: MsgType
pattern BEGIN_SPAN = MsgType 1
pattern END_SPAN = MsgType 2
pattern TAG = MsgType 3
pattern EVENT = MsgType 4
pattern SET_PARENT_CONTEXT = MsgType 5
pattern SET_TRACE_ID = MsgType 6
pattern SET_SPAN_ID = MsgType 7
pattern DECLARE_INSTRUMENT = MsgType 8
pattern METRIC_CAPTURE = MsgType 9

{-# INLINE maxMsgLen #-}
maxMsgLen :: Int
maxMsgLen = shift 2 16

{-# INLINE otelMagic #-}
otelMagic :: Int
otelMagic = v .|. t .|. o
  where
    !v = shift 3 16
    !t = shift (ord 'T') 8
    !o = ord 'O'

{-# INLINE header #-}
header :: MsgType -> Builder
header (MsgType msgType) = word32LE $ fromIntegral h
  where
    !h = m .|. otelMagic
    !m = shift ((fromIntegral msgType) :: Int) $ shift 3 3

headerSize :: Int
headerSize = fromIntegral $ LBS.length $ toLazyByteString (header TAG <> word64LE 0)

{-# INLINE checkSize #-}
checkSize :: Int -> m -> m
checkSize s next = do
  let !exceed = s + headerSize - maxMsgLen
  if exceed > 0
    then error $ "eventlog message size exceed 64k by " ++ show exceed
    else next

{-# INLINE nextLocalSpan #-}
nextLocalSpan :: MonadIO m => m SpanInFlight
nextLocalSpan = liftIO $ (SpanInFlight . fromIntegral . hashUnique) <$> newUnique

{-# INLINE nextInstrumentId #-}
nextInstrumentId :: MonadIO m => m InstrumentId
nextInstrumentId = liftIO $ (fromIntegral . hashUnique) <$> newUnique

-- These functions all depend on the binary eventlog to be useful.
#if __GLASGOW_HASKELL__ >= 808
{-# INLINE builder_beginSpan #-}
builder_beginSpan :: SpanInFlight -> BS.ByteString -> Builder
builder_beginSpan (SpanInFlight u) operation =
  header BEGIN_SPAN <> word64LE u <> byteString operation

{-# INLINE builder_endSpan #-}
builder_endSpan :: SpanInFlight -> Builder
builder_endSpan (SpanInFlight u) = header END_SPAN <> word64LE u

{-# INLINE builder_key_value #-}
builder_key_value :: MsgType -> SpanInFlight -> BS.ByteString -> BS.ByteString -> Builder
builder_key_value msg (SpanInFlight u) k v =
  let klen = fromIntegral $ BS.length k
      vlen = fromIntegral $ BS.length v
   in header msg <> word64LE u <> word32LE klen <> word32LE vlen <> byteString k <> byteString v

{-# INLINE builder_setTag #-}
builder_setTag :: SpanInFlight -> BS.ByteString -> BS.ByteString -> Builder
builder_setTag = builder_key_value TAG

{-# INLINE builder_addEvent #-}
builder_addEvent :: SpanInFlight -> BS.ByteString -> BS.ByteString -> Builder
builder_addEvent = builder_key_value EVENT

{-# INLINE builder_setParentSpanContext #-}
builder_setParentSpanContext :: SpanInFlight -> SpanContext -> Builder
builder_setParentSpanContext (SpanInFlight u) (SpanContext (SId sid) (TId tid)) =
  header SET_PARENT_CONTEXT <> word64LE u <> word64LE sid <> word64LE tid

{-# INLINE builder_setTraceId #-}
builder_setTraceId :: SpanInFlight -> TraceId -> Builder
builder_setTraceId (SpanInFlight u) (TId tid) = header SET_TRACE_ID <> word64LE u <> word64LE tid

{-# INLINE builder_setSpanId #-}
builder_setSpanId :: SpanInFlight -> SpanId -> Builder
builder_setSpanId (SpanInFlight u) (SId sid) = header SET_SPAN_ID <> word64LE u <> word64LE sid

{-# INLINE builder_declareInstrument #-}
builder_declareInstrument :: Instrument s a m -> Builder
builder_declareInstrument instrument =
  header DECLARE_INSTRUMENT <>
  int8 (instrumentTag instrument) <>
  word64LE (instrumentId instrument) <>
  byteString (instrumentName instrument)

{-# INLINE builder_captureMetric #-}
builder_captureMetric :: InstrumentId -> Int -> Builder
builder_captureMetric iId v =
  header METRIC_CAPTURE <>
  word64LE iId <>
  int64LE (fromIntegral v)

{-# INLINE traceBuilder #-}
traceBuilder :: MonadIO m => Builder -> m ()
traceBuilder = liftIO . traceBinaryEventIO . LBS.toStrict . toLazyByteString
#endif

-- For use with human-readable eventlog

beginSpan' :: SpanInFlight -> String -> String
beginSpan' (SpanInFlight u64) operation =
  printf "ot2 begin span %d %s" u64 operation

endSpan' :: SpanInFlight -> String
endSpan' (SpanInFlight u64) = printf "ot2 end span %d" u64

setTag' :: SpanInFlight -> String -> BS8.ByteString -> String
setTag' (SpanInFlight u64) k v =
  printf "ot2 set tag %d %s %s" u64 k (BS8.unpack v)

addEvent' :: SpanInFlight -> String -> BS8.ByteString -> String
addEvent' (SpanInFlight u64) k v =
  printf "ot2 add event %d %s %s" u64 k (BS8.unpack v)

setParentSpanContext' :: SpanInFlight -> SpanContext -> String
setParentSpanContext' (SpanInFlight u64) (SpanContext (SId sid) (TId tid)) =
  (printf "ot2 set parent %d %016x %016x" u64 tid sid)

setTraceId' :: SpanInFlight -> TraceId -> String
setTraceId' (SpanInFlight u64) (TId tid) =
  printf "ot2 set traceid %d %016x" u64 tid

setSpanId' :: SpanInFlight -> SpanId -> String
setSpanId' (SpanInFlight u64) (SId sid) =
  printf "ot2 set spanid %d %016x" u64 sid

createInstrument' :: MI.Instrument s a m -> String
createInstrument' i = printf "ot2 metric create %s %016x %s" (instrumentTagStr i) (instrumentId i) (BS8.unpack $ instrumentName i)

writeMetric' :: InstrumentId -> Int -> String
writeMetric' iid v = printf "ot2 metric capture %016x %s" iid (show v)

{-# INLINE instrumentTag #-}
instrumentTag :: Instrument s a m -> Int8
instrumentTag Counter {} = 1
instrumentTag UpDownCounter {} = 2
instrumentTag ValueRecorder {} = 3
instrumentTag SumObserver {} = 4
instrumentTag UpDownSumObserver {} = 5
instrumentTag ValueObserver {} = 6

{-# INLINE instrumentTagStr #-}
instrumentTagStr :: Instrument s a m -> String
instrumentTagStr Counter {} = "Counter"
instrumentTagStr UpDownCounter {} = "UpDownCounter"
instrumentTagStr ValueRecorder {} = "ValueRecorder"
instrumentTagStr SumObserver {} = "SumObserver"
instrumentTagStr UpDownSumObserver {} = "UpDownSumObserver"
instrumentTagStr ValueObserver {} = "ValueObserver"
