module LogEventSerializer where

import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import GHC.RTS.Events
import OpenTelemetry.Common
import OpenTelemetry.EventlogStreaming_Internal
import qualified OpenTelemetry.Eventlog_Internal as BE

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

logEventToUserBinaryMessage :: OpenTelemetryEventlogEvent -> EventInfo
logEventToUserBinaryMessage = UserBinaryMessage . logEventToBs
