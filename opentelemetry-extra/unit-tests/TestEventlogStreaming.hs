{-# LANGUAGE OverloadedStrings #-}

module TestEventlogStreaming where

import Arbitrary ()
import Data.Function
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.List (foldl', sort)
import GHC.RTS.Events
import LogEventSerializer
import OpenTelemetry.Common hiding (Event)
import OpenTelemetry.Eventlog
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.SpanContext
import Test.QuickCheck
import TextShow

processEvents :: [Event] -> State -> (State, [Span], [Metric])
processEvents events st0 = foldl' go (st0, [], []) events
  where
    go (st, sps, ms) e =
      let (st', sps', ms') = processEvent e st
       in (st', sps' <> sps, ms <> ms')

prop_number_of_spans_in_eventlog_is_number_of_spans_exported :: [(SpanInFlight, SpanName)] -> Bool
prop_number_of_spans_in_eventlog_is_number_of_spans_exported spans =
  let input_events = concatMap convert spans
      convert (span_serial_number, spanName) =
        [ Event 0 (logEventToUserBinaryMessage $ BeginSpanEv span_serial_number spanName) (Just 0),
          Event 42 (logEventToUserBinaryMessage $ EndSpanEv span_serial_number) (Just 0)
        ]
      (_end_state, emitted_spans, _emitted_metrics) = processEvents input_events (initialState 0 (error "randomGen seed"))
   in length emitted_spans == length spans

prop_user_specified_span_ids_are_used :: [(SpanInFlight, SpanId, SpanName)] -> Bool
prop_user_specified_span_ids_are_used spans =
  let input_events = concatMap convert spans
      convert (span_serial_number, sid, span_name) =
        [ Event 0 (logEventToUserBinaryMessage $ BeginSpanEv span_serial_number span_name) (Just 0),
          Event 1 (logEventToUserBinaryMessage $ SetSpanEv span_serial_number sid) (Just 0),
          Event 42 (logEventToUserBinaryMessage $ EndSpanEv span_serial_number) (Just 0)
        ]
      (_end_state, emitted_spans, _emitted_metrics) = processEvents input_events (initialState 0 (error "randomGen seed"))
   in sort (map (\(_, x, _) -> x) spans) == sort (map spanId emitted_spans)

prop_user_specified_things_are_used :: [(SpanInFlight, SpanId, SpanName)] -> Property
prop_user_specified_things_are_used spans =
  distinct (map (\(serial, _, _) -> serial) spans)
    ==> distinct (map (\(_, span_id, _) -> span_id) spans)
    ==> classify (length spans > 1) "multiple spans"
    $ let input_events = concatMap convert spans
          convert (span_serial_number, sid@(SId sid'), span_name) =
            [ Event 0 (logEventToUserBinaryMessage $ BeginSpanEv  span_serial_number span_name) (Just 0),
              Event 1 (logEventToUserBinaryMessage $ SetSpanEv span_serial_number sid) (Just 0),
              Event 2 (logEventToUserBinaryMessage $ TagEv span_serial_number (TagName "color") (TagVal $ showt sid')) (Just 0),
              Event 3 (logEventToUserBinaryMessage $ SetTraceEv span_serial_number (TId sid')) (Just 0),
              Event 4 (logEventToUserBinaryMessage $ EventEv span_serial_number (EventName "message") (EventVal $ showt sid')) (Just 0),
              Event 42 (logEventToUserBinaryMessage $ EndSpanEv span_serial_number) (Just 0)
            ]
          (_end_state, emitted_spans, _emitted_metrics) = processEvents input_events (initialState 0 (error "randomGen seed"))
          corresponding_span_was_emitted (_serial, SId sid, _thread_id) =
            emitted_spans
              & filter
                ( \sp ->
                    and
                      [ spanId sp == SId sid,
                        spanTraceId sp == TId sid,
                        HM.lookup (TagName "color") (spanTags sp)
                              == Just (StringTagValue $ TagVal (showt sid)),
                        any
                          (\SpanEvent {..} -> (spanEventKey == EventName "message")
                                              && (spanEventValue == (EventVal (showt sid))))
                          (spanEvents sp)
                      ]
                )
              & length
              & (== (1 :: Int))
       in conjoin $ map corresponding_span_was_emitted spans

prop_parenting_works_when_everything_is_on_one_thread_and_nested_properly :: [SpanInFlight] -> Property
prop_parenting_works_when_everything_is_on_one_thread_and_nested_properly serials =
  distinct serials ==> length serials > 1
    ==> let input_events = prelude <> map convert_begin serials <> map convert_end (reverse serials)
            prelude = [Event 0 (CreateThread 1) (Just 0)]
            convert_begin serial =
              Event 1 (logEventToUserBinaryMessage $ BeginSpanEv serial $ SpanName $ showt serial) (Just 0)
            convert_end serial =
              Event 42 (logEventToUserBinaryMessage $ EndSpanEv serial) (Just 0)
            (_end_state, emitted_spans, _emitted_metrics) = processEvents input_events (initialState 0 (error "randomGen seed"))
            check_relationship (sp, psp) = spanParentId sp === Just (spanId psp)
         in conjoin $ map check_relationship (zip (tail emitted_spans) emitted_spans)

prop_beginning_a_span_on_one_thread_and_ending_on_another_is_fine :: SpanInFlight -> ThreadId -> ThreadId -> Int -> Int -> Property
prop_beginning_a_span_on_one_thread_and_ending_on_another_is_fine serial begin_tid end_tid begin_cap end_cap =
  let input_events =
        [ Event 0 (CreateThread begin_tid) (Just begin_cap),
          Event 1 (RunThread begin_tid) (Just begin_cap),
          Event 2 (logEventToUserBinaryMessage $ BeginSpanEv serial $ SpanName $ showt serial) (Just begin_cap),
          Event 3 (CreateThread end_tid) (Just end_cap),
          Event 4 (RunThread end_tid) (Just end_cap),
          Event 5 (logEventToUserBinaryMessage $ EndSpanEv serial) (Just end_cap)
        ]
      (end_state, emitted_spans, _emitted_metrics) = processEvents input_events (initialState 0 (error "randomGen seed"))
   in conjoin
        [ length emitted_spans === 1,
          spanOperation (head emitted_spans) === showt serial,
          spanStartedAt (head emitted_spans) === 2,
          spanFinishedAt (head emitted_spans) === 5,
          spanThreadId (head emitted_spans) === begin_tid,
          True === null (spans end_state),
          True === null (serial2sid end_state)
        ]

distinct :: (Eq a, Hashable a) => [a] -> Bool
distinct things = length things == HS.size (foldMap HS.singleton things)
