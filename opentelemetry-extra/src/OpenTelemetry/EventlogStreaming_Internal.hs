{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.EventlogStreaming_Internal where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import Data.Maybe
import qualified Data.Text as T
import Data.Word
import GHC.RTS.Events
import GHC.RTS.Events.Incremental
import OpenTelemetry.Common hiding (Event, Timestamp)
import OpenTelemetry.Debug
import OpenTelemetry.Exporter
import OpenTelemetry.Handler
import OpenTelemetry.Parser
import OpenTelemetry.SpanContext
import qualified OpenTelemetry.Binary.Parser as BP
import OpenTelemetry.Text.Parser

import System.IO
import qualified System.Random.SplitMix as R

data WatDoOnEOF = StopOnEOF | SleepAndRetryOnEOF


work :: WatDoOnEOF -> Timestamp -> Exporter Span -> Handle -> IO ()
work wat_do_on_eof origin_timestamp exporter input = do
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


initialState :: Word64 -> R.SMGen -> State
initialState timestamp = S timestamp mempty mempty mempty mempty mempty


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
        (UserMessage {msg}, _, fromMaybe 1 -> tid) ->
            fromMaybe (st, [])
                      $ fmap (\ev' -> handle ev' st (tid, now, m_trace_id))
                            (parseText (T.words msg))
        (UserBinaryMessage {payload}, _, fromMaybe 1 -> tid) ->
            case BP.parse payload of
              Nothing -> (st, [])
              Just ev' -> handle ev' st (tid, now, m_trace_id)
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

-- modifyAllSpans :: (Span -> Span) -> State -> State
-- modifyAllSpans f st =
--   st
--     { spanStacks =
--         fmap
--           (\(sp :| sps) -> (f sp :| sps))
--           (spanStacks st)
--     }


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
