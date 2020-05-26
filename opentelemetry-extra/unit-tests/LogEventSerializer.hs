module LogEventSerializer where

import Data.ByteString.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.RTS.Events
import qualified OpenTelemetry.Binary.Eventlog as BE
import OpenTelemetry.Common
import OpenTelemetry.EventlogStreaming_Internal
import qualified OpenTelemetry.Eventlog as E

txt2Bs :: T.Text -> LBS.ByteString
txt2Bs = LBS.fromStrict . TE.encodeUtf8

logEventToBs :: OpenTelemetryEventlogEvent -> BS.ByteString
logEventToBs = LBS.toStrict . toLazyByteString . logEventToBuilder

logEventToBuilder :: OpenTelemetryEventlogEvent -> Builder
logEventToBuilder (BeginSpanEv localId (SpanName name)) =
    BE.beginSpan' localId . txt2Bs $ name

logEventToBuilder (EndSpanEv localId) = BE.endSpan' localId
logEventToBuilder (TagEv localId (TagName k) (TagVal v)) =
    BE.setTag' localId (txt2Bs k) $ txt2Bs v
logEventToBuilder (EventEv localId (EventName k) (EventVal v)) =
    BE.addEvent' localId (txt2Bs k) $ txt2Bs v
logEventToBuilder (SetParentEv locId spnCtx) = BE.setParentSpanContext' locId spnCtx
logEventToBuilder (SetTraceEv localId traceId) = BE.setTraceId' localId traceId
logEventToBuilder (SetSpanEv localId spanId') = BE.setSpanId' localId spanId'


logEventToTxt :: OpenTelemetryEventlogEvent -> T.Text
logEventToTxt = T.pack . logEventToStr

logEventToStr :: OpenTelemetryEventlogEvent -> String
logEventToStr (BeginSpanEv localId (SpanName name)) =
    E.beginSpan' localId . T.unpack $ name
logEventToStr (EndSpanEv localId) = E.endSpan' localId
logEventToStr (TagEv localId (TagName k) (TagVal v)) =
    let (k', v') = (T.unpack k, TE.encodeUtf8 v)
     in E.setTag' localId k' v'
logEventToStr (EventEv localId (EventName k) (EventVal v)) =
    let (k', v') = (T.unpack k, TE.encodeUtf8 v)
     in E.addEvent' localId k' v'

logEventToStr (SetParentEv locId spnCtx) = E.setParentSpanContext' locId spnCtx
logEventToStr (SetTraceEv localId traceId) = E.setTraceId' localId traceId
logEventToStr (SetSpanEv localId spanId') = E.setSpanId' localId spanId'

logEventToUserMessage :: OpenTelemetryEventlogEvent -> EventInfo
logEventToUserMessage = UserMessage . logEventToTxt

logEventToUserBinaryMessage :: OpenTelemetryEventlogEvent -> EventInfo
logEventToUserBinaryMessage = UserBinaryMessage . logEventToBs
