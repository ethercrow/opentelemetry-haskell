{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module OpenTelemetry.Spans where

import Data.ByteString as BS
import Data.ByteString.Builder as BSB
import Data.ByteString.Lazy as LBS
import Data.Coerce
import qualified Data.HashMap.Strict as HM

import Data.ProtoLens (defMessage)
import Data.Word
import Lens.Micro
import OpenTelemetry.Attribute
import OpenTelemetry.Common as OTC
import OpenTelemetry.SpanContext
import OpenTelemetry.Resource
import Proto.Opentelemetry.Proto.Common.V1.Common as C
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields as C
import Proto.Opentelemetry.Proto.Trace.V1.Trace as T
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields as T


instrLib :: C.InstrumentationLibrary
instrLib = defMessage
            & C.name .~ "OpenTelemetry-haskell"
            & C.version .~ "0.4.0"


spansToLibSpans :: [OTC.Span] -> T.InstrumentationLibrarySpans
spansToLibSpans sps =
    defMessage
      & T.instrumentationLibrary .~ instrLib
      & T.spans .~ (fmap span2Span sps)


w8 :: Word64 -> BSB.Builder
w8 = word64LE

sbs :: BSB.Builder -> BS.ByteString
sbs = toStrict . toLazyByteString

maybeSpanToBytes :: Maybe SpanId -> BS.ByteString
maybeSpanToBytes Nothing = mempty
maybeSpanToBytes (Just (SId parentSid)) = sbs $ w8 parentSid

newtype AttributeTags = AttributeTags (HM.HashMap TagName TagValue)

instance ConversionTo AttributeTags [C.AttributeKeyValue]  where
    convertTo (AttributeTags m) = fmap tuple2Attr $ HM.toList m
            where tuple2Attr (TagName k, v) = tagToAttribute k v
newtype AttributeSpanEvent = AttributeSpanEvent SpanEvent

instance ConversionTo AttributeSpanEvent T.Span'Event where
    convertTo (AttributeSpanEvent SpanEvent{..}) =
        defMessage
        & T.name .~ (coerce spanEventKey)
        & T.droppedAttributesCount .~ 0
        & T.timeUnixNano .~ spanEventTimestamp
        & T.attributes .~ [strAttr "text" $ coerce spanEventValue]

span2Span :: OTC.Span -> T.Span
span2Span Span{..} =
    case spanContext of
      SpanContext (SId sid) (TId tid) ->
          defMessage
           & T.traceId .~ (sbs $ w8 sid <> w8 sid)
           & T.spanId .~ (sbs $ w8 tid)
           -- & T.traceState .~
           & T.parentSpanId .~ (maybeSpanToBytes spanParentId)
           & T.name .~ spanOperation
           -- & T.kind .~ not enough info
           & T.startTimeUnixNano .~ spanStartedAt
           & T.endTimeUnixNano .~ spanFinishedAt
           & T.attributes .~ (convertTo $ AttributeTags spanTags)
           & T.events .~ (fmap (convertTo . AttributeSpanEvent) spanEvents)
           & T.droppedAttributesCount .~ 0
           & T.links .~ []
           & T.droppedLinksCount .~ 0
           & T.status .~ (defMessage & T.code .~ T.Status'Ok)
