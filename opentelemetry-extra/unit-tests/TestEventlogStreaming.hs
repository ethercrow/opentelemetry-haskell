{-# LANGUAGE OverloadedStrings #-}

module TestEventlogStreaming where

import Data.Function
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.List (foldl', sort)
import qualified Data.Text as T
import Data.Word
import GHC.RTS.Events
import OpenTelemetry.Common hiding (Event)
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.SpanContext
import Test.QuickCheck
import Text.Printf
import TextShow

instance Arbitrary SpanId where
  arbitrary = SId <$> arbitrary

processEvents :: [Event] -> State -> (State, [Span])
processEvents events st0 = foldl' go (st0, []) events
  where
    go (st, sps) e =
      let (st', sps') = processEvent e st
       in (st', sps' <> sps)

prop_number_of_spans_in_eventlog_is_number_of_spans_exported :: [(Word64, Int)] -> Bool
prop_number_of_spans_in_eventlog_is_number_of_spans_exported spans =
  let input_events = concatMap convert spans
      convert (span_serial_number, thread_id) =
        [ Event 0 (UserMessage {msg = T.pack $ printf "ot2 begin span %d %d" span_serial_number thread_id}) (Just 0),
          Event 42 (UserMessage {msg = T.pack $ printf "ot2 end span %d" span_serial_number}) (Just 0)
        ]
      (_end_state, emitted_spans) = processEvents input_events (initialState 0 (error "randomGen seed"))
   in length emitted_spans == length spans

prop_user_specified_span_ids_are_used :: [(Word64, SpanId, Int)] -> Bool
prop_user_specified_span_ids_are_used spans =
  let input_events = concatMap convert spans
      convert (span_serial_number, SId sid, thread_id) =
        [ Event 0 (UserMessage {msg = T.pack $ printf "ot2 begin span %d %d" span_serial_number thread_id}) (Just 0),
          Event 1 (UserMessage {msg = T.pack $ printf "ot2 set spanid %d %016x" span_serial_number sid}) (Just 0),
          Event 42 (UserMessage {msg = T.pack $ printf "ot2 end span %d" span_serial_number}) (Just 0)
        ]
      (_end_state, emitted_spans) = processEvents input_events (initialState 0 (error "randomGen seed"))
   in sort (map (\(_, x, _) -> x) spans) == sort (map spanId emitted_spans)

prop_user_specified_things_are_used :: [(Word64, SpanId, Int)] -> Property
prop_user_specified_things_are_used spans =
  distinct (map (\(serial, _, _) -> serial) spans)
    ==> distinct (map (\(_, span_id, _) -> span_id) spans)
    ==> classify (length spans > 1) "multiple spans"
    $ let input_events = concatMap convert spans
          convert (span_serial_number, SId sid, thread_id) =
            [ Event 0 (UserMessage {msg = T.pack $ printf "ot2 begin span %d %d" span_serial_number thread_id}) (Just 0),
              Event 1 (UserMessage {msg = T.pack $ printf "ot2 set spanid %d %016x" span_serial_number sid}) (Just 0),
              Event 2 (UserMessage {msg = T.pack $ printf "ot2 set tag %d color %d" span_serial_number sid}) (Just 0),
              Event 3 (UserMessage {msg = T.pack $ printf "ot2 set traceid %d %016x" span_serial_number sid}) (Just 0),
              Event 4 (UserMessage {msg = T.pack $ printf "ot2 add event %d message %d" span_serial_number sid}) (Just 0),
              -- Event 4 (UserMessage {msg = T.pack $ printf "ot2 add event %d %016x" span_serial_number sid}) (Just 0),
              Event 42 (UserMessage {msg = T.pack $ printf "ot2 end span %d" span_serial_number}) (Just 0)
            ]
          (_end_state, emitted_spans) = processEvents input_events (initialState 0 (error "randomGen seed"))
          corresponding_span_was_emitted (_serial, SId sid, _thread_id) =
            emitted_spans
              & filter
                ( \sp ->
                    and
                      [ spanId sp == SId sid,
                        spanTraceId sp == TId sid,
                        HM.lookup "color" (spanTags sp) == Just (StringTagValue (showt sid)),
                        any
                          (\SpanEvent {..} -> spanEventKey == "message" && spanEventValue == showt sid)
                          (spanEvents sp)
                      ]
                )
              & length
              & (== (1 :: Int))
       in conjoin $ map corresponding_span_was_emitted spans

prop_parenting_works_when_everything_is_on_one_thread_and_nested_properly :: [Word64] -> Property
prop_parenting_works_when_everything_is_on_one_thread_and_nested_properly serials =
  distinct serials ==> length serials > 1
    ==> let input_events = prelude <> map convert_begin serials <> map convert_end (reverse serials)
            prelude = [Event 0 (CreateThread 1) (Just 0)]
            convert_begin serial =
              Event 1 (UserMessage {msg = T.pack $ printf "ot2 begin span %d %d" serial serial}) (Just 0)
            convert_end serial =
              Event 42 (UserMessage {msg = T.pack $ printf "ot2 end span %d" serial}) (Just 0)
            (_end_state, emitted_spans) = processEvents input_events (initialState 0 (error "randomGen seed"))
            check_relationship (sp, psp) = spanParentId sp === Just (spanId psp)
         in conjoin $ map check_relationship (zip (tail emitted_spans) emitted_spans)

distinct :: (Eq a, Hashable a) => [a] -> Bool
distinct things = length things == HS.size (foldMap HS.singleton things)
