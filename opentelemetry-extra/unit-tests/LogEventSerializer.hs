module LogEventSerializer where

import Data.ByteString.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified OpenTelemetry.Binary.Eventlog as BE
import OpenTelemetry.Common
import qualified OpenTelemetry.Eventlog as E
import OpenTelemetry.Handler

txt2Bs :: T.Text -> LBS.ByteString
txt2Bs = LBS.fromStrict . TE.encodeUtf8

logEventToBs :: LogEvent -> BS.ByteString
logEventToBs =  LBS.toStrict . toLazyByteString . logEventToBuilder

logEventToBuilder :: LogEvent -> Builder
logEventToBuilder (BeginSpanEv localId (SpanName name)) =
    BE.beginSpan' localId . txt2Bs $ name

logEventToBuilder (EndSpanEv localId) = BE.endSpan' localId
logEventToBuilder (TagEv localId (TagName k) (TagVal v)) =
    let [k', v'] = map txt2Bs [k, v]
     in BE.setTag' localId k' v'
logEventToBuilder (EventEv localId (EventName k) (EventVal v)) =
    let [k', v'] = map txt2Bs [k, v]
     in BE.addEvent' localId k' v'
logEventToBuilder (SetParentEv locId spnCtx) = BE.setParentSpanContext' locId spnCtx
logEventToBuilder (SetTraceEv localId traceId) = BE.setTraceId' localId traceId
logEventToBuilder (SetSpanEv localId spanId) = BE.setSpanId' localId spanId
