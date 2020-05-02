{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.EventlogStreaming_Internal where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Data.Word
import GHC.RTS.Events
import GHC.RTS.Events.Incremental
import GHC.Stack
import OpenTelemetry.Common hiding (Event, Timestamp)
import qualified OpenTelemetry.Common as OTel
import OpenTelemetry.Debug
import OpenTelemetry.Exporter
import OpenTelemetry.SpanContext
import System.IO
import qualified System.Random.SplitMix as R
import Text.Printf

work :: Timestamp -> Exporter Span -> Handle -> IO ()
work origin_timestamp exporter input = do
  d_ "Starting the eventlog reader"
  smgen <- R.initSMGen -- TODO(divanov): seed the random generator with something more random than current time
  go (initialState origin_timestamp smgen) decodeEventLog
  d_ "no more work"
  where
    go s (Produce event next) = do
      case evSpec event of
        Shutdown {} -> do
          d_ "Shutdown-like event detected"
        CapDelete {} -> do
          d_ "Shutdown-like event detected"
        CapsetDelete {} -> do
          d_ "Shutdown-like event detected"
        _ -> do
          -- d_ "go Produce"
          dd_ "event" (evTime event, evCap event, evSpec event)
          let (s', sps) = processEvent event s
          _ <- export exporter sps
          -- print s'
          mapM_ (d_ . ("emit " <>) . show) sps
          go s' next
    go s d@(Consume consume) = do
      -- d_ "go Consume"
      eof <- hIsEOF input
      case eof of
        False -> do
          chunk <- B.hGetSome input 4096
          -- printf "chunk = %d bytes\n" (B.length chunk)
          if B.null chunk
            then do
              -- d_ "chunk is null"
              threadDelay 1000 -- TODO(divanov): remove the sleep by replacing the hGetSome with something that blocks until data is available
              go s d
            else do
              -- d_ "chunk is not null"
              go s $ consume chunk
        True -> do
          d_ "EOF"
          threadDelay 1000
          go s d
    go _ (Done _) = do
      d_ "go Done"
      pure ()
    go _ (Error _leftover err) = do
      d_ "go Error"
      d_ err

data State = S
  { originTimestamp :: Timestamp,
    threadMap :: IM.IntMap ThreadId,
    spanStacks :: HM.HashMap ThreadId (NonEmpty Span),
    traceMap :: HM.HashMap ThreadId TraceId,
    specificSpans :: HM.HashMap Word64 Span,
    randomGen :: R.SMGen
  }
  deriving (Show)

initialState :: Word64 -> R.SMGen -> State
initialState timestamp = S timestamp mempty mempty mempty mempty

processEvent :: Event -> State -> (State, [Span])
processEvent (Event ts ev m_cap) st@(S {..}) =
  let now = originTimestamp + ts
      m_thread_id = m_cap >>= flip IM.lookup threadMap
      m_trace_id = m_thread_id >>= flip HM.lookup traceMap
   in case (ev, m_cap, m_thread_id) of
        (WallClockTime {sec, nsec}, _, _) -> (st {originTimestamp = sec * 1_000_000_000 + fromIntegral nsec - ts}, [])
        (CreateThread new_tid, _, _) ->
          case m_trace_id of
            Just trace_id -> (st {traceMap = HM.insert new_tid trace_id traceMap}, [])
            _ -> (st, [])
        (RunThread tid, Just cap, _) ->
          (st {threadMap = IM.insert cap tid threadMap}, [])
        (StopThread _ tstatus, Just cap, _)
          | isTerminalThreadStatus tstatus -> (st {threadMap = IM.delete cap threadMap}, [])
        (StartGC, _, _) -> (pushGCSpans st now, [])
        (GCStatsGHC {gen}, _, _) -> (modifyAllSpans (setTag "gen" gen) st, [])
        (EndGC, _, _) -> popSpansAcrossAllThreads now st
        (HeapAllocated {allocBytes}, _, Just tid) ->
          (modifySpan tid (addEvent now "heap_alloc_bytes" (showT allocBytes)) st, [])
        (UserMessage {msg}, _, fromMaybe 1 -> tid) -> case T.words msg of
          ("ot1" : "begin" : "specific" : "span" : trace_id_text : span_id_text : name) ->
            let trace_id = TId (read ("0x" <> T.unpack trace_id_text))
                span_id = SId (read ("0x" <> T.unpack span_id_text))
             in beginSpecificSpan trace_id span_id (T.intercalate " " name) now st
          ("ot1" : "end" : "specific" : "span" : trace_id_text : span_id_text : _) ->
            let trace_id = TId (read ("0x" <> T.unpack trace_id_text))
                span_id = SId (read ("0x" <> T.unpack span_id_text))
             in endSpecificSpan span_id now st
          ("ot1" : "begin" : "span" : name) ->
            (pushSpan tid (T.intercalate " " name) now st, [])
          ("ot1" : "end" : "span" : _) -> popSpan tid now st
          ("ot1" : "set" : "tag" : k : v) -> (modifySpan tid (setTag k (T.unwords v)) st, [])
          ["ot1", "set", "traceid", trace_id_text] ->
            let trace_id = TId (read ("0x" <> T.unpack trace_id_text))
             in ( (modifySpan tid (setTraceId trace_id) st)
                    { traceMap = HM.insert tid trace_id traceMap
                    },
                  []
                )
          ["ot1", "set", "spanid", span_id] ->
            (modifySpan tid (setSpanId (SId (read ("0x" <> T.unpack span_id)))) st, [])
          ["ot1", "set", "parent", trace_id_text, span_id_text] ->
            let trace_id = TId (read ("0x" <> T.unpack trace_id_text))
                sid = SId (read ("0x" <> T.unpack span_id_text))
             in ( (modifySpan tid (setParent trace_id sid) st)
                    { traceMap = HM.insert tid trace_id traceMap
                    },
                  []
                )
          ("ot1" : "add" : "event" : k : v) -> (modifySpan tid (addEvent now k (T.unwords v)) st, [])
          ("ot1" : rest) -> error $ printf "Unrecognized %s" (show rest)
          _ -> (st, [])
        _ -> (st, [])

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

setTraceId :: TraceId -> Span -> Span
setTraceId tid sp =
  sp
    { spanContext = SpanContext (spanId sp) tid
    }

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

modifyAllSpans :: (Span -> Span) -> State -> State
modifyAllSpans f st =
  st
    { spanStacks =
        fmap
          (\(sp :| sps) -> (f sp :| sps))
          (spanStacks st)
    }

modifySpan :: HasCallStack => ThreadId -> (Span -> Span) -> State -> State
modifySpan tid f st =
  st
    { spanStacks =
        HM.update (\(sp :| sps) -> Just (f sp :| sps)) tid (spanStacks st)
    }

beginSpecificSpan :: TraceId -> SpanId -> T.Text -> OTel.Timestamp -> State -> (State, [Span])
beginSpecificSpan trace_id span_id@(SId s) name timestamp st =
  case HM.lookup s (specificSpans st) of
    Just sp -> (st {specificSpans = HM.delete s (specificSpans st)}, [sp {spanStartedAt = timestamp, spanOperation = name, spanContext = SpanContext span_id trace_id}])
    Nothing ->
      (st {specificSpans = HM.insert s sp (specificSpans st)}, [])
      where
        sp =
          Span
            { spanContext = SpanContext span_id trace_id,
              spanOperation = name,
              spanStartedAt = timestamp,
              spanFinishedAt = 0,
              spanTags = mempty,
              spanEvents = mempty,
              spanStatus = OK,
              spanParentId = Nothing
            }

endSpecificSpan :: SpanId -> OTel.Timestamp -> State -> (State, [Span])
endSpecificSpan span_id@(SId s) timestamp st =
  case HM.lookup s (specificSpans st) of
    Just sp -> (st {specificSpans = HM.delete s (specificSpans st)}, [sp {spanFinishedAt = timestamp}])
    Nothing ->
      (st {specificSpans = HM.insert s sp (specificSpans st)}, [])
      where
        sp =
          Span
            { spanContext = SpanContext span_id (TId 0),
              spanOperation = "unknown",
              spanStartedAt = 0,
              spanFinishedAt = timestamp,
              spanTags = mempty,
              spanEvents = mempty,
              spanStatus = OK,
              spanParentId = Nothing
            }

pushSpan :: HasCallStack => ThreadId -> T.Text -> OTel.Timestamp -> State -> State
pushSpan tid name timestamp st = st {spanStacks = new_stacks, randomGen = new_randomGen, traceMap = new_traceMap}
  where
    maybe_parent = NE.head <$> HM.lookup tid (spanStacks st)
    new_stacks = HM.alter f tid (spanStacks st)
    f Nothing = Just $ sp :| []
    f (Just sps) = Just $ cons sp sps
    (sid, new_randomGen) = R.nextWord64 (randomGen st)
    (new_traceMap, trace_id) = case (maybe_parent, HM.lookup tid (traceMap st)) of
      (Just parent, _) -> (traceMap st, spanTraceId parent)
      (_, Just trace_id') -> (traceMap st, trace_id')
      _ -> let new_trace_id = TId sid in (HM.insert tid new_trace_id (traceMap st), new_trace_id)
    sp =
      Span
        { spanContext = SpanContext (SId sid) trace_id,
          spanOperation = name,
          spanStartedAt = timestamp,
          spanFinishedAt = 0,
          spanTags = HM.singleton "tid" (IntTagValue $ fromIntegral tid),
          spanEvents = mempty,
          spanStatus = OK,
          spanParentId = spanId <$> maybe_parent
        }

popSpan :: HasCallStack => ThreadId -> OTel.Timestamp -> State -> (State, [Span])
popSpan tid timestamp st = (st {spanStacks = new_stacks, traceMap = new_traceMap}, [sp {spanFinishedAt = timestamp}])
  where
    sp :| new_stack = fromMaybe (error $ printf "popSpan: missing span stack for thread %d" tid) $ HM.lookup tid (spanStacks st)
    (new_traceMap, new_stacks) = case new_stack of
      [] -> (HM.delete tid (traceMap st), HM.delete tid (spanStacks st))
      x : xs -> (traceMap st, HM.insert tid (x :| xs) (spanStacks st))

pushGCSpans :: HasCallStack => State -> OTel.Timestamp -> State
pushGCSpans st timestamp = foldr go st tids
  where
    tids = HM.keys (spanStacks st)
    go tid = pushSpan tid "gc" timestamp

popSpansAcrossAllThreads :: HasCallStack => OTel.Timestamp -> State -> (State, [Span])
popSpansAcrossAllThreads timestamp st = foldr go (st, []) tids
  where
    tids = HM.keys (spanStacks st)
    go tid (st', sps) =
      let (st'', sps') = popSpan tid timestamp st'
       in (st'', sps' <> sps)

isTerminalThreadStatus :: ThreadStopStatus -> Bool
isTerminalThreadStatus HeapOverflow = True
isTerminalThreadStatus StackOverflow = True
isTerminalThreadStatus ThreadFinished = True
isTerminalThreadStatus _ = False

showT :: Show a => a -> T.Text
showT = T.pack . show
