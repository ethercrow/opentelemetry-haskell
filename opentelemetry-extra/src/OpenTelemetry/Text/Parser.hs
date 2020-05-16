{-# LANGUAGE OverloadedStrings #-}
module OpenTelemetry.Text.Parser where

import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text as T
import Data.Word
import GHC.Stack
import OpenTelemetry.Common
-- import OpenTelemetry.Handler
import qualified OpenTelemetry.Parser as P
import OpenTelemetry.SpanContext
import Text.Printf

emitSpan :: Word64 -> SpanId -> P.State -> (P.State, Span)
emitSpan serial span_id st =
  case (HM.lookup serial $ P.serial2sid st, HM.lookup span_id $ P.spans st) of
    (Just span_id', Just sp)
      | span_id == span_id' ->
        ( st
            { P.spans = HM.delete span_id $ P.spans st,
              P.serial2sid = HM.delete serial $ P.serial2sid st,
              P.thread2sid = HM.update (const $ spanParentId sp)
                           (spanThreadId sp) (P.thread2sid st)
            },
          sp
        )
    _ -> error "emitSpan invariants violated"


modifySpan :: HasCallStack => SpanId -> (Span -> Span) -> P.State -> P.State
modifySpan sid f st = st {P.spans = HM.adjust f sid (P.spans st)}

setParent :: TraceId -> SpanId -> Span -> Span
setParent ptid psid sp =
  sp
    { spanParentId = Just psid,
      spanContext = SpanContext (spanId sp) ptid
    }

addEvent :: Timestamp -> T.Text -> T.Text -> Span -> Span
addEvent ts k v sp = sp {spanEvents = new_events}
  where
    new_events = ev : spanEvents sp
    ev = SpanEvent ts k v

setTraceId :: TraceId -> Span -> Span
setTraceId tid sp =
  sp
    { spanContext = SpanContext (spanId sp) tid
    }

setTag :: ToTagValue v => T.Text -> v -> Span -> Span
setTag k v sp =
  sp
    { spanTags = HM.insert k (toTagValue v) (spanTags sp)
    }

setSpanId :: SpanId -> Span -> Span
setSpanId sid sp =
  sp
    { spanContext = SpanContext sid (spanTraceId sp)
    }

inventSpanId :: Word64 -> P.State -> (P.State, SpanId)
inventSpanId serial st = (st {P.serial2sid = HM.insert serial sid (P.serial2sid st)}, sid)
  where
    sid = SId serial -- TODO: use random generator instead

parseText :: [T.Text]
          -> P.State
          -> Word32 -- ThreadId
          -> Timestamp
          -> Maybe TraceId
          -> (P.State, [Span])
parseText msgWords st tid now m_trace_id =
    case msgWords of
      ("ot2" : "begin" : "span" : serial_text : name) ->
        let serial = read (T.unpack serial_text)
            operation = T.intercalate " " name
         in case HM.lookup serial (P.serial2sid st) of
              Nothing ->
                let (st', span_id) = inventSpanId serial st
                    parent = HM.lookup tid (P.thread2sid st)
                    sp =
                      Span
                        { spanContext = SpanContext span_id (fromMaybe (TId 42) m_trace_id),
                          spanOperation = operation,
                          spanThreadId = tid,
                          spanStartedAt = now,
                          spanFinishedAt = 0,
                          spanTags = mempty,
                          spanEvents = mempty,
                          spanStatus = OK,
                          spanParentId = parent
                        }
                 in ( st'
                        { P.spans = HM.insert span_id sp (P.spans st),
                          P.thread2sid = HM.insert tid span_id (P.thread2sid st)
                        },
                      []
                    )
              Just span_id ->
                let (st', sp) = emitSpan serial span_id st
                 in (st', [sp {spanOperation = operation, spanStartedAt = now, spanThreadId = tid}])
      ["ot2", "end", "span", serial_text] ->
        let serial = read (T.unpack serial_text)
         in case HM.lookup serial $ P.serial2sid st of
              Nothing ->
                let (st', span_id) = inventSpanId serial st
                    parent = HM.lookup tid (P.thread2sid st)
                    sp =
                      Span
                        { spanContext = SpanContext span_id (fromMaybe (TId 42) m_trace_id),
                          spanOperation = "",
                          spanThreadId = tid,
                          spanStartedAt = 0,
                          spanFinishedAt = now,
                          spanTags = mempty,
                          spanEvents = mempty,
                          spanStatus = OK,
                          spanParentId = parent
                        }
                 in ( st'
                        { P.spans = HM.insert span_id sp $ P.spans st,
                          P.thread2sid = HM.insert tid span_id $ P.thread2sid st
                        },
                      []
                    )
              Just span_id ->
                let (st', sp) = emitSpan serial span_id st
                 in (st', [sp {spanFinishedAt = now}])
      ("ot2" : "set" : "tag" : serial_text : k : v) ->
        let serial = read (T.unpack serial_text)
         in case HM.lookup serial $ P.serial2sid st of
              Nothing -> error $ "set tag: span id not found for serial" <> T.unpack serial_text
              Just span_id -> (modifySpan span_id (setTag k (T.unwords v)) st, [])
      ["ot2", "set", "traceid", serial_text, trace_id_text] ->
        let serial = read (T.unpack serial_text)
            trace_id = TId (read ("0x" <> T.unpack trace_id_text))
         in case HM.lookup serial $ P.serial2sid st of
              Nothing -> error $ "set traceid: span id not found for serial" <> T.unpack serial_text
              Just span_id ->
                ( (modifySpan span_id (setTraceId trace_id) st)
                  { P.traceMap = HM.insert tid trace_id $ P.traceMap st
                  },
                  []
                )
      ["ot2", "set", "spanid", serial_text, new_span_id_text] ->
        let serial = read (T.unpack serial_text)
         in case HM.lookup serial $ P.serial2sid st of
              Just old_span_id -> (modifySpan old_span_id (setSpanId (SId (read ("0x" <> T.unpack new_span_id_text)))) st, [])
              Nothing -> error $ "set spanid " <> T.unpack serial_text <> " " <> T.unpack new_span_id_text <> ": span id not found"
      ["ot2", "set", "parent", serial_text, trace_id_text, parent_span_id_text] ->
        let trace_id = TId (read ("0x" <> T.unpack trace_id_text))
            serial = read (T.unpack serial_text)
            psid = SId (read ("0x" <> T.unpack parent_span_id_text))
         in case HM.lookup serial $ P.serial2sid st of
              Just span_id ->
                ( (modifySpan span_id (setParent trace_id psid) st)
                   { P.traceMap = HM.insert tid trace_id (P.traceMap st)
                   },
                  []
                )
              Nothing -> error $ "set parent: span not found for serial " <> show serial
      ("ot2" : "add" : "event" : serial_text : k : v) ->
        let serial = read (T.unpack serial_text)
         in case HM.lookup serial (P.serial2sid st) of
              Just span_id -> (modifySpan span_id (addEvent now k (T.unwords v)) st, [])
              Nothing -> error $ "add event: span not found for serial " <> show serial
      ("ot2" : rest) -> error $ printf "Unrecognized %s" (show rest)
      _ -> (st, [])
