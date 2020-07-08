module LogEventSerializer where

import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import GHC.RTS.Events
import OpenTelemetry.Common
import OpenTelemetry.EventlogStreaming_Internal
import qualified OpenTelemetry.Eventlog_Internal as BE
import OpenTelemetry.Metrics_Internal

logEventToBs :: OpenTelemetryEventlogEvent -> BS.ByteString
logEventToBs = LBS.toStrict . toLazyByteString . logEventToBuilder

logEventToBuilder :: OpenTelemetryEventlogEvent -> Builder
logEventToBuilder (BeginSpanEv localId (SpanName name)) =
  BE.builder_beginSpan localId . TE.encodeUtf8 $ name
logEventToBuilder (EndSpanEv localId) = BE.builder_endSpan localId
logEventToBuilder (TagEv localId (TagName k) (TagVal v)) =
  BE.builder_setTag localId (TE.encodeUtf8 k) (TE.encodeUtf8 v)
logEventToBuilder (EventEv localId (EventName k) (EventVal v)) =
  BE.builder_addEvent localId (TE.encodeUtf8 k) (TE.encodeUtf8 v)
logEventToBuilder (SetParentEv locId spnCtx) = BE.builder_setParentSpanContext locId spnCtx
logEventToBuilder (SetTraceEv localId traceId) = BE.builder_setTraceId localId traceId
logEventToBuilder (SetSpanEv localId spanId') = BE.builder_setSpanId localId spanId'
logEventToBuilder (DeclareInstrumentEv tag iId iName) = case declareInstrumentEvToInstrument tag iId iName of
  SomeInstrument i -> BE.builder_declareInstrument i
logEventToBuilder (MetricCaptureEv i val) = BE.builder_captureMetric i val

logEventToUserBinaryMessage :: OpenTelemetryEventlogEvent -> EventInfo
logEventToUserBinaryMessage = UserBinaryMessage . logEventToBs

declareInstrumentEvToInstrument :: InstrumentType -> InstrumentId -> InstrumentName -> SomeInstrument
declareInstrumentEvToInstrument CounterType iid name = SomeInstrument $ Counter name iid
declareInstrumentEvToInstrument UpDownCounterType iid name = SomeInstrument $ UpDownCounter name iid
declareInstrumentEvToInstrument ValueRecorderType iid name = SomeInstrument $ ValueRecorder name iid
declareInstrumentEvToInstrument SumObserverType iid name = SomeInstrument $ SumObserver name iid
declareInstrumentEvToInstrument UpDownSumObserverType iid name = SomeInstrument $ UpDownSumObserver name iid
declareInstrumentEvToInstrument ValueObserverType iid name = SomeInstrument $ ValueObserver name iid
