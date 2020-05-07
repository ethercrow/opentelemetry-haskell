module TestEventlogStreaming where

import Data.List (foldl', sort)
import qualified Data.Text as T
import Data.Word
import GHC.RTS.Events
import OpenTelemetry.Common hiding (Event)
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.SpanContext
import Test.QuickCheck
import Text.Printf

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
      convert (span_serial_number, span_id@(SId sid), thread_id) =
        [ Event 0 (UserMessage {msg = T.pack $ printf "ot2 begin span %d %d" span_serial_number thread_id}) (Just 0),
          Event 1 (UserMessage {msg = T.pack $ printf "ot2 set spanid %d %016x" span_serial_number sid}) (Just 0),
          Event 42 (UserMessage {msg = T.pack $ printf "ot2 end span %d" span_serial_number}) (Just 0)
        ]
      (_end_state, emitted_spans) = processEvents input_events (initialState 0 (error "randomGen seed"))
   in sort (map (\(_, x, _) -> x) spans) == sort (map spanId emitted_spans)
