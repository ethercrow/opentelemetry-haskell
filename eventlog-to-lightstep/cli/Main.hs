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
      -- print (evCap event, evSpec event)
      let (s', sps) = processEvent event s
      export exporter sps
      -- mapM_ (putStrLn . ("emit " <>) . show) sps
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
        randomGen :: R.SMGen
      }
  deriving (Show)

initialState :: R.SMGen -> State
initialState = S 0 mempty mempty

processEvent :: Event -> State -> (State, [Span])
processEvent (Event ts ev mcap) st@(S o tm ss r) =
  let now = o + ts
      mtid = mcap >>= flip IM.lookup tm
   in case (ev, mcap, mtid) of
        (RunThread tid, Just cap, _) ->
          -- TODO(divanov): maintain the thread ancestry
          (S o (IM.insert cap tid tm) ss r, [])
        (StopThread tid tstatus, Just cap, _)
          | isTerminalThreadStatus tstatus -> (S o (IM.delete cap tm) ss r, [])
        (StartGC, _, _) -> (st, []) -- TODO(divanov): push a gc span on every stack
        (GCStatsGHC {gen}, _, _) -> (modifyAllSpans (setTag "gen" gen) st, [])
        (EndGC, _, _) -> (st, []) -- TODO(divanov): pop the gc span from every stack
        (UserMessage {msg}, Just cap, _) -> case words msg of
          ["ot1", "begin", "span", name] -> (pushSpan st cap name now, [])
          ["ot1", "end", "span"] -> popSpan st cap now
          ["ot1", "set", "tag", k, v] -> (modifySpan st cap (setTag (T.pack k) v), [])
          ["ot1", "add", "event", k, v] -> (modifySpan st cap (addEvent (ts, o) (T.pack k) v), [])
          _ -> (st, [])
        _ -> (st, [])

setTag :: ToTagValue v => T.Text -> v -> Span -> Span
setTag k v sp =
  sp
    { spanTags = HM.insert k (toTagValue v) (spanTags sp)
    }

addEvent :: ToTagValue v => (Timestamp, Timestamp) -> T.Text -> v -> Span -> Span
addEvent ts k v sp = sp {spanEvents = new_events}
  where
    new_events = spanEvents sp -- TODO(divanov): implement

modifyAllSpans :: (Span -> Span) -> State -> State
modifyAllSpans f st =
  st
    { spanStacks =
        fmap
          (\(sp :| sps) -> (f sp :| sps))
          (spanStacks st)
    }

modifySpan :: HasCallStack => State -> Int -> (Span -> Span) -> State
modifySpan st cap f =
  st
    { spanStacks =
        let tid = threadMap st IM.! cap
         in HM.update (\(sp :| sps) -> Just (f sp :| sps)) tid (spanStacks st)
    }

pushSpan :: HasCallStack => State -> Int -> String -> OTel.Timestamp -> State
pushSpan st cap name timestamp = st {spanStacks = new_stacks, randomGen = r'}
  where
    maybe_parent = NE.head <$> HM.lookup tid (spanStacks st)
    new_stacks = HM.alter f tid (spanStacks st)
    f Nothing = Just $ sp :| []
    f (Just sps) = Just $ cons sp sps
    tid = threadMap st IM.! cap
    (sid, r') = R.nextWord64 (randomGen st)
    sp =
      Span
        { spanContext = SpanContext (SId sid) (maybe (TId sid) spanTraceId maybe_parent),
          spanOperation = T.pack name,
          spanStartedAt = timestamp,
          spanFinishedAt = 0,
          spanTags = mempty,
          spanEvents = mempty,
          spanStatus = OK,
          spanParentId = spanId <$> maybe_parent
        }

popSpan :: HasCallStack => State -> Int -> OTel.Timestamp -> (State, [Span])
popSpan st cap now = (st {spanStacks = new_stacks}, [sp {spanFinishedAt = now}])
  where
    sp :| new_stack = spanStacks st HM.! tid
    new_stacks = case new_stack of
      [] -> HM.delete tid (spanStacks st)
      (x : xs) -> HM.insert tid (x :| xs) (spanStacks st)
    tid = threadMap st IM.! cap

isTerminalThreadStatus :: ThreadStopStatus -> Bool
isTerminalThreadStatus HeapOverflow = True
isTerminalThreadStatus StackOverflow = True
isTerminalThreadStatus ThreadFinished = True
isTerminalThreadStatus _ = False
