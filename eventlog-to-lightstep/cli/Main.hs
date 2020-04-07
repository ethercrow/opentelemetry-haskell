{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import GHC.RTS.Events
import GHC.RTS.Events.Incremental
import GHC.Stack
import OpenTelemetry.Common hiding (Event, Timestamp)
import qualified OpenTelemetry.Common as OTel
import OpenTelemetry.LightStep.Config
import OpenTelemetry.LightStep.ZipkinExporter
import System.Environment
import System.IO
import qualified System.Random.SplitMix as R
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      printf "Sending %s to LightStep...\n" path
      Just lsConfig <- getEnvConfig
      exporter <- createLightStepSpanExporter lsConfig
      withFile path ReadMode (work exporter)
      shutdown exporter
      putStrLn "\nAll done.\n"
    _ -> do
      -- TODO(divanov): figure out how to get an eventlog of a running process
      putStrLn "Usage:"
      putStrLn "  eventlog-to-lightstep <program.eventlog>"

work :: Exporter Span -> Handle -> IO ()
work exporter input = do
  smgen <- R.initSMGen -- TODO(divanov): seed the random generator with something more random than current time

  -- TODO(divanov): get the origin timestamp

  go (initialState smgen) decodeEventLog
  where
    go s (Produce event next) = do
      print (evCap event, evSpec event)
      let (s', sps) = processEvent event s
      export exporter sps
      print s'
      mapM_ (putStrLn . ("emit " <>) . show) sps
      go s' next
    go s d@(Consume consume) = do
      eof <- hIsEOF input
      when (not eof) $ do
        chunk <- B.hGetSome input 4096
        if B.null chunk
          then do
            threadDelay 1000 -- TODO(divanov): remove the sleep by replacing the hGetSome with something that blocks until data is available
            go s d
          else go s $ consume chunk
    go s (Done _) = pure ()
    go s (Error _leftover err) = do
      putStrLn err

data State
  = S
      { originTimestamp :: Timestamp,
        threadMap :: IM.IntMap ThreadId,
        spanStacks :: HM.HashMap ThreadId (NonEmpty Span),
        traceMap :: HM.HashMap ThreadId TraceId,
        randomGen :: R.SMGen
      }
  deriving (Show)

initialState :: R.SMGen -> State
initialState = S 0 mempty mempty mempty

processEvent :: Event -> State -> (State, [Span])
processEvent (Event ts ev m_cap) st@(S {..}) =
  let now = originTimestamp + ts
      m_thread_id = m_cap >>= flip IM.lookup threadMap
      m_trace_id = m_thread_id >>= flip HM.lookup traceMap
   in case (ev, m_cap, m_thread_id) of
        (CreateThread new_tid, _, _) ->
          case m_trace_id of
            Just trace_id -> (st {traceMap = HM.insert new_tid trace_id traceMap}, [])
            _ -> (st, [])
        (RunThread tid, Just cap, _) ->
          (st { threadMap = IM.insert cap tid threadMap}, [])
        (StopThread tid tstatus, Just cap, _)
          | isTerminalThreadStatus tstatus -> (st { threadMap = IM.delete cap threadMap}, [])
        (StartGC, _, _) -> (st, []) -- TODO(divanov): push a gc span on every stack
        (GCStatsGHC {gen}, _, _) -> (modifyAllSpans (setTag "gen" gen) st, [])
        (EndGC, _, _) -> (st, []) -- TODO(divanov): pop the gc span from every stack
        (HeapAllocated {allocBytes}, _, Just tid) ->
          (modifySpan st tid (addEvent now "heap_alloc_bytes" (showT allocBytes)), [])
        (UserMessage {msg}, _, Just tid) -> case T.words msg of
          ["ot1", "begin", "span", name] -> (pushSpan st tid name now, [])
          ["ot1", "end", "span"] -> popSpan st tid now
          ["ot1", "set", "tag", k, v] -> (modifySpan st tid (setTag k v), [])
          ["ot1", "add", "event", k, v] -> (modifySpan st tid (addEvent now k v), [])
          _ -> (st, [])
        _ -> (st, [])

setTag :: ToTagValue v => T.Text -> v -> Span -> Span
setTag k v sp =
  sp
    { spanTags = HM.insert k (toTagValue v) (spanTags sp)
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

modifySpan :: HasCallStack => State -> ThreadId -> (Span -> Span) -> State
modifySpan st tid f =
  st
    { spanStacks =
         HM.update (\(sp :| sps) -> Just (f sp :| sps)) tid (spanStacks st)
    }

pushSpan :: HasCallStack => State -> ThreadId -> T.Text -> OTel.Timestamp -> State
pushSpan st tid name timestamp = st {spanStacks = new_stacks, randomGen = new_randomGen, traceMap = new_traceMap}
  where
    maybe_parent = NE.head <$> HM.lookup tid (spanStacks st)
    new_stacks = HM.alter f tid (spanStacks st)
    f Nothing = Just $ sp :| []
    f (Just sps) = Just $ cons sp sps
    (sid, new_randomGen) = R.nextWord64 (randomGen st)
    (new_traceMap, trace_id) = case (maybe_parent, HM.lookup tid (traceMap st)) of
      (Just parent, _) -> (traceMap st, spanTraceId parent)
      (_, Just trace_id) -> (traceMap st, trace_id)
      _ -> let new_trace_id = TId sid in (HM.insert tid new_trace_id (traceMap st), new_trace_id)
    sp =
      Span
        { spanContext = SpanContext (SId sid) trace_id,
          spanOperation = name,
          spanStartedAt = timestamp,
          spanFinishedAt = 0,
          spanTags = mempty,
          spanEvents = mempty,
          spanStatus = OK,
          spanParentId = spanId <$> maybe_parent
        }

popSpan :: HasCallStack => State -> ThreadId -> OTel.Timestamp -> (State, [Span])
popSpan st tid now = (st {spanStacks = new_stacks, traceMap = new_traceMap}, [sp {spanFinishedAt = now}])
  where
    sp :| new_stack = spanStacks st HM.! tid
    (new_traceMap, new_stacks) = case new_stack of
      [] -> (HM.delete tid (traceMap st), HM.delete tid (spanStacks st))
      (x : xs) -> (traceMap st, HM.insert tid (x :| xs) (spanStacks st))

isTerminalThreadStatus :: ThreadStopStatus -> Bool
isTerminalThreadStatus HeapOverflow = True
isTerminalThreadStatus StackOverflow = True
isTerminalThreadStatus ThreadFinished = True
isTerminalThreadStatus _ = False

showT :: Show a => a -> T.Text
showT = T.pack . show
