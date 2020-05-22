{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.EventlogStreaming_Internal where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.IntMap as IM
import Data.Maybe
import qualified Data.Text as T
import Data.Word
import GHC.RTS.Events
import GHC.RTS.Events.Incremental
import GHC.Stack
import OpenTelemetry.Common hiding (Event, Timestamp)
import OpenTelemetry.Debug
import OpenTelemetry.Exporter
import OpenTelemetry.SpanContext
import System.IO
import qualified System.Random.SplitMix as R
import Text.Printf

data WatDoOnEOF = StopOnEOF | SleepAndRetryOnEOF

instance Hashable SpanId

data EventSource
  = EventLogHandle Handle WatDoOnEOF
  | EventLogFilename FilePath

work :: Timestamp -> Exporter Span -> EventSource -> IO ()
work origin_timestamp exporter source = do
  d_ "Starting the eventlog reader"
  smgen <- R.initSMGen -- TODO(divanov): seed the random generator with something more random than current time
  let state0 = initialState origin_timestamp smgen
  case source of
   EventLogFilename path  -> do
     readEventLogFromFile path >>= \case
       Right (dat -> Data {events}) -> do
         let go s [] = pure ()
             go s (e : es) = do
               dd_ "event" (evTime e, evCap e, evSpec e)
               case processEvent e s of
                 (s', sps) -> do
                   mapM_ (d_ . ("emit " <>) . show) sps
                   _ <- export exporter sps
                   go s' es
         go state0 $ sortEvents events
       Left err -> do
         putStrLn err
   EventLogHandle input wat_do_on_eof -> do
    let go s (Produce event next) = do
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
              case wat_do_on_eof of
                StopOnEOF -> pure ()
                SleepAndRetryOnEOF -> do
                  threadDelay 1000
                  go s d
        go _ (Done _) = do
          d_ "go Done"
          pure ()
        go _ (Error _leftover err) = do
          d_ "go Error"
          d_ err
    go state0 decodeEventLog
  d_ "no more work"

data State = S
  { originTimestamp :: Timestamp,
    threadMap :: IM.IntMap ThreadId,
    spans :: HM.HashMap SpanId Span,
    traceMap :: HM.HashMap ThreadId TraceId,
    serial2sid :: HM.HashMap Word64 SpanId,
    thread2sid :: HM.HashMap ThreadId SpanId,
    randomGen :: R.SMGen
  }
  deriving (Show)

initialState :: Word64 -> R.SMGen -> State
initialState timestamp = S timestamp mempty mempty mempty mempty mempty

inventSpanId :: Word64 -> State -> (State, SpanId)
inventSpanId serial st = (st {serial2sid = HM.insert serial sid (serial2sid st)}, sid)
  where
    sid = SId serial -- TODO: use random generator instead

processEvent :: Event -> State -> (State, [Span])
processEvent (Event ts ev m_cap) st@(S {..}) =
  let now = originTimestamp + ts
      m_thread_id = m_cap >>= flip IM.lookup threadMap
      m_trace_id = m_thread_id >>= flip HM.lookup traceMap
   in case (ev, m_cap, m_thread_id) of
        (WallClockTime {sec, nsec}, _, _) ->
          (st {originTimestamp = sec * 1_000_000_000 + fromIntegral nsec - ts}, [])
        (CreateThread new_tid, _, _) ->
          let trace_id = case m_trace_id of
                Just t -> t
                Nothing -> TId originTimestamp -- TODO: something more random
           in (st {traceMap = HM.insert new_tid trace_id traceMap}, [])
        (RunThread tid, Just cap, _) ->
          (st {threadMap = IM.insert cap tid threadMap}, [])
        (StopThread tid tstatus, Just cap, _)
          | isTerminalThreadStatus tstatus ->
            ( st
                { threadMap = IM.delete cap threadMap,
                  traceMap = HM.delete tid traceMap
                },
              []
            )
        -- (StartGC, _, _) ->
        --   (pushGCSpans st now, [])
        -- (GCStatsGHC {gen}, _, _) ->
        --   (modifyAllSpans (setTag "gen" gen) st, [])
        -- (EndGC, _, _) ->
        --   popSpansAcrossAllThreads now st
        -- (HeapAllocated {allocBytes}, _, Just tid) ->
        --   (modifySpan tid (addEvent now "heap_alloc_bytes" (showT allocBytes)) st, [])
        (UserMessage {msg}, _, fromMaybe 1 -> tid) -> case T.words msg of
          ("ot2" : "begin" : "span" : serial_text : name) ->
            let serial = read (T.unpack serial_text)
                operation = T.intercalate " " name
             in case HM.lookup serial serial2sid of
                  Nothing ->
                    let (st', span_id) = inventSpanId serial st
                        parent = HM.lookup tid thread2sid
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
                            { spans = HM.insert span_id sp spans,
                              thread2sid = HM.insert tid span_id thread2sid
                            },
                          []
                        )
                  Just span_id ->
                    let (st', sp) = emitSpan serial span_id st
                     in (st', [sp {spanOperation = operation, spanStartedAt = now, spanThreadId = tid}])
          ["ot2", "end", "span", serial_text] ->
            let serial = read (T.unpack serial_text)
             in case HM.lookup serial serial2sid of
                  Nothing ->
                    let (st', span_id) = inventSpanId serial st
                        parent = HM.lookup tid thread2sid
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
                            { spans = HM.insert span_id sp spans,
                              thread2sid = HM.insert tid span_id thread2sid
                            },
                          []
                        )
                  Just span_id ->
                    let (st', sp) = emitSpan serial span_id st
                     in (st', [sp {spanFinishedAt = now}])
          ("ot2" : "set" : "tag" : serial_text : k : v) ->
            let serial = read (T.unpack serial_text)
             in case HM.lookup serial serial2sid of
                  Nothing -> error $ "set tag: span id not found for serial " <> T.unpack serial_text
                  Just span_id -> (modifySpan span_id (setTag k (T.unwords v)) st, [])
          ["ot2", "set", "traceid", serial_text, trace_id_text] ->
            let serial = read (T.unpack serial_text)
                trace_id = TId (read ("0x" <> T.unpack trace_id_text))
             in case HM.lookup serial serial2sid of
                  Nothing -> error $ "set traceid: span id not found for serial " <> T.unpack serial_text
                  Just span_id ->
                    ( (modifySpan span_id (setTraceId trace_id) st)
                        { traceMap = HM.insert tid trace_id traceMap
                        },
                      []
                    )
          ["ot2", "set", "spanid", serial_text, new_span_id_text] ->
            let serial = read (T.unpack serial_text)
             in case HM.lookup serial serial2sid of
                  Just old_span_id -> (modifySpan old_span_id (setSpanId (SId (read ("0x" <> T.unpack new_span_id_text)))) st, [])
                  Nothing -> error $ "set spanid " <> T.unpack serial_text <> " " <> T.unpack new_span_id_text <> ": span id not found"
          ["ot2", "set", "parent", serial_text, trace_id_text, parent_span_id_text] ->
            let trace_id = TId (read ("0x" <> T.unpack trace_id_text))
                serial = read (T.unpack serial_text)
                psid = SId (read ("0x" <> T.unpack parent_span_id_text))
             in case HM.lookup serial serial2sid of
                  Just span_id ->
                    ( (modifySpan span_id (setParent trace_id psid) st)
                        { traceMap = HM.insert tid trace_id traceMap
                        },
                      []
                    )
                  Nothing -> error $ "set parent: span not found for serial " <> show serial
          ("ot2" : "add" : "event" : serial_text : k : v) ->
            let serial = read (T.unpack serial_text)
             in case HM.lookup serial serial2sid of
                  Just span_id -> (modifySpan span_id (addEvent now k (T.unwords v)) st, [])
                  Nothing -> error $ "add event: span not found for serial " <> show serial
          ("ot2" : rest) -> error $ printf "Unrecognized %s" (show rest)
          _ -> (st, [])
        _ -> (st, [])

-- beginSpan :: TraceId -> SpanId -> T.Text -> OTel.Timestamp -> State -> (State, [Span])
-- beginSpan trace_id span_id@(SId s) name timestamp st =
--   case HM.lookup s (specificSpans st) of
--     Just sp -> (st {specificSpans = HM.delete s (specificSpans st)}, [sp {spanStartedAt = timestamp, spanOperation = name, spanContext = SpanContext span_id trace_id}])
--     Nothing ->
--       (st {specificSpans = HM.insert s sp (specificSpans st)}, [])
--       where
--         sp =
--           Span
--             { spanContext = SpanContext span_id trace_id,
--               spanOperation = name,
--               spanStartedAt = timestamp,
--               spanFinishedAt = 0,
--               spanTags = mempty,
--               spanEvents = mempty,
--               spanStatus = OK,
--               spanParentId = Nothing
--             }

-- endSpan :: Word64 -> OTel.Timestamp -> State -> (State, [Span])
-- endSpan serial timestamp st =
--   case HM.lookup serial (serial2sid st) of
--     Just span_id@(SId s) -> (st {specificSpans = HM.delete s (specificSpans st)}, [sp {spanFinishedAt = timestamp}])
--     Nothing ->
--       (st {specificSpans = HM.insert s sp (specificSpans st)}, [])
--       where
--         sp =
--           Span
--             { spanContext = SpanContext span_id (TId 0),
--               spanOperation = "unknown",
--               spanStartedAt = 0,
--               spanFinishedAt = timestamp,
--               spanTags = mempty,
--               spanEvents = mempty,
--               spanStatus = OK,
--               spanParentId = Nothing
--             }

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

-- modifyAllSpans :: (Span -> Span) -> State -> State
-- modifyAllSpans f st =
--   st
--     { spanStacks =
--         fmap
--           (\(sp :| sps) -> (f sp :| sps))
--           (spanStacks st)
--     }

modifySpan :: HasCallStack => SpanId -> (Span -> Span) -> State -> State
modifySpan sid f st = st {spans = HM.adjust f sid (spans st)}

-- pushSpan :: HasCallStack => ThreadId -> T.Text -> OTel.Timestamp -> Maybe Word64 -> State -> State
-- pushSpan tid name timestamp serial st = st {spanStacks = new_stacks, randomGen = new_randomGen, traceMap = new_traceMap}
--   where
--     maybe_parent = NE.head <$> HM.lookup tid (spanStacks st)
--     new_stacks = HM.alter f tid (spanStacks st)
--     f Nothing = Just $ sp :| []
--     f (Just sps) = Just $ cons sp sps
--     (sid, new_randomGen) = R.nextWord64 (randomGen st)
--     (new_traceMap, trace_id) = case (maybe_parent, HM.lookup tid (traceMap st)) of
--       (Just parent, _) -> (traceMap st, spanTraceId parent)
--       (_, Just trace_id') -> (traceMap st, trace_id')
--       _ -> let new_trace_id = TId sid in (HM.insert tid new_trace_id (traceMap st), new_trace_id)
--     sp =
--       Span
--         { spanContext = SpanContext (SId sid) trace_id,
--           spanOperation = name,
--           spanStartedAt = timestamp,
--           spanFinishedAt = 0,
--           spanTags = HM.singleton "tid" (IntTagValue $ fromIntegral tid),
--           spanEvents = mempty,
--           spanStatus = OK,
--           spanParentId = spanId <$> maybe_parent
--         }

-- popSpan :: HasCallStack => ThreadId -> OTel.Timestamp -> State -> (State, [Span])
-- popSpan tid timestamp st = (st {spanStacks = new_stacks, traceMap = new_traceMap}, [sp {spanFinishedAt = timestamp}])
--   where
--     sp :| new_stack = fromMaybe (error $ printf "popSpan: missing span stack for thread %d" tid) $ HM.lookup tid (spanStacks st)
--     (new_traceMap, new_stacks) = case new_stack of
--       [] -> (HM.delete tid (traceMap st), HM.delete tid (spanStacks st))
--       x : xs -> (traceMap st, HM.insert tid (x :| xs) (spanStacks st))

-- pushGCSpans :: HasCallStack => State -> OTel.Timestamp -> State
-- pushGCSpans st timestamp = foldr go st tids
--   where
--     tids = HM.keys (spanStacks st)
--     go tid = pushSpan tid "gc" timestamp Nothing

-- popSpansAcrossAllThreads :: HasCallStack => OTel.Timestamp -> State -> (State, [Span])
-- popSpansAcrossAllThreads timestamp st = foldr go (st, []) tids
--   where
--     tids = HM.keys (spanStacks st)
--     go tid (st', sps) =
--       let (st'', sps') = popSpan tid timestamp st'
--        in (st'', sps' <> sps)

isTerminalThreadStatus :: ThreadStopStatus -> Bool
isTerminalThreadStatus HeapOverflow = True
isTerminalThreadStatus StackOverflow = True
isTerminalThreadStatus ThreadFinished = True
isTerminalThreadStatus _ = False

showT :: Show a => a -> T.Text
showT = T.pack . show

emitSpan :: Word64 -> SpanId -> State -> (State, Span)
emitSpan serial span_id st@S {..} =
  case (HM.lookup serial serial2sid, HM.lookup span_id spans) of
    (Just span_id', Just sp)
      | span_id == span_id' ->
        ( st
            { spans = HM.delete span_id spans,
              serial2sid = HM.delete serial serial2sid,
              thread2sid = HM.update (const $ spanParentId sp) (spanThreadId sp) thread2sid
            },
          sp
        )
    _ -> error "emitSpan invariants violated"
