{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Handler where

import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text as T
import Data.Word
import GHC.Stack
import OpenTelemetry.Binary.Eventlog (SpanInFlight (..))
import OpenTelemetry.Common
import qualified OpenTelemetry.Parser as P
import OpenTelemetry.SpanContext


data LogEvent where
    BeginSpanEv :: SpanInFlight
                -> T.Text
                -> LogEvent
    EndSpanEv   :: SpanInFlight
                -> LogEvent
    TagEv       :: SpanInFlight
                -> T.Text
                -> T.Text
                -> LogEvent
    EventEv     :: SpanInFlight
                -> T.Text
                -> T.Text
                -> LogEvent
    SetParentEv :: SpanInFlight
                -> SpanContext
                -> LogEvent
    SetTraceEv  :: SpanInFlight
                -> TraceId
                -> LogEvent
    SetSpanEv   :: SpanInFlight
                -> SpanId
                -> LogEvent

handle :: LogEvent
       -> P.State
       -> (Word32, Timestamp, Maybe TraceId)
       -> (P.State, [Span])
handle m st (tid, now, m_trace_id) =
    case m of
      EventEv (SpanInFlight serial) k v ->
          case HM.lookup serial (P.serial2sid st) of
              Just span_id -> (modifySpan span_id (addEvent now k v) st, [])
              Nothing -> error $ "add event: span not found for serial " <> show serial
      SetParentEv (SpanInFlight serial) (SpanContext psid trace_id) ->
          case HM.lookup serial $ P.serial2sid st of
              Just span_id ->
                ( (modifySpan span_id (setParent trace_id psid) st)
                   { P.traceMap = HM.insert tid trace_id (P.traceMap st)
                   },
                  []
                )
              Nothing -> error $ "set parent: span not found for serial " <> show serial
      SetSpanEv (SpanInFlight serial) span_id ->
          case HM.lookup serial $ P.serial2sid st of
              Just old_span_id -> (modifySpan old_span_id (setSpanId span_id) st, [])
              Nothing -> error $ "set spanid " <> show serial <> " " <> show span_id <> ": span id not found"
      SetTraceEv (SpanInFlight serial) trace_id ->
          case HM.lookup serial $ P.serial2sid st of
              Nothing -> error $ "set traceid: span id not found for serial" <> show serial
              Just span_id ->
                ( (modifySpan span_id (setTraceId trace_id) st)
                  { P.traceMap = HM.insert tid trace_id $ P.traceMap st
                  },
                  []
                )
      TagEv (SpanInFlight serial) k v ->
          case HM.lookup serial $ P.serial2sid st of
              Nothing -> error $ "set tag: span id not found for serial" <> show serial
              Just span_id -> (modifySpan span_id (setTag k v) st, [])
      EndSpanEv (SpanInFlight serial) ->
          case HM.lookup serial $ P.serial2sid st of
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
      BeginSpanEv (SpanInFlight serial) operation ->
         case HM.lookup serial (P.serial2sid st) of
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
