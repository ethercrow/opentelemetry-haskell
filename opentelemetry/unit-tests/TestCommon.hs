module TestCommon where

import Data.Word
import OpenTelemetry.Common

prop_tracer_push_does_something :: Word64 -> Word64 -> Int -> Bool
prop_tracer_push_does_something sid tid threadid =
  let tracer0 = Tracer mempty
      sp0 = emptySpan {spanContext = SpanContext (SId sid) (TId tid)}
      tracer1 = tracerPushSpan tracer0 threadid sp0
   in tracer0 /= tracer1

prop_tracer_push_pop :: Word64 -> Word64 -> Int -> Bool
prop_tracer_push_pop sid tid threadid =
  let tracer0 = Tracer mempty
      sp0 = emptySpan {spanContext = SpanContext (SId sid) (TId tid)}
      tracer1 = tracerPushSpan tracer0 threadid sp0
      (Just sp1, tracer2) = tracerPopSpan tracer1 threadid
   in tracer0 == tracer2 && sp0 == sp1
