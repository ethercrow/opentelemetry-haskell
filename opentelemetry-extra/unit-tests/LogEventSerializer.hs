module LogEventSerializer where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified OpenTelemetry.Binary.Eventlog as BE
import qualified OpenTelemetry.Eventlog as E
import OpenTelemetry.Handler

txt2Bs :: T.Text -> LBS.ByteString
txt2Bs = LBS.fromStrict . TE.encodeUtf8

logEventToBs :: LogEvent -> Builder
logEventToBs (BeginSpanEv localId name) =
    BE.beginSpan' localId . txt2Bs $ name

logEventToBs (EndSpanEv localId) = BE.endSpan' localId
logEventToBs (TagEv localId k v) =
    let [k', v'] = map txt2Bs [k, v]
     in BE.setTag' localId k' v'
logEventToBs (EventEv localId k v) =
    let [k', v'] = map txt2Bs [k, v]
     in BE.addEvent' localId k' v'
logEventToBs (SetParentEv locId spnCtx) = BE.setParentSpanContext' locId spnCtx
logEventToBs (SetTraceEv localId traceId) = BE.setTraceId' localId traceId
logEventToBs (SetSpanEv localId spanId) = BE.setSpanId' localId spanId
