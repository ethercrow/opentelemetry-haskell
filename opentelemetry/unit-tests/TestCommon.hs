{-# LANGUAGE OverloadedStrings #-}

module TestCommon where

import Data.Word
import OpenTelemetry.Common

mkTestSpan :: Word64 -> Word64 -> Span
mkTestSpan sid tid =
  Span
    { spanContext = SpanContext (SId sid) (TId tid),
      spanStartedAt = 0,
      spanFinishedAt = 10,
      spanTags = mempty,
      spanEvents = mempty,
      spanStatus = OK,
      spanOperation = "foo",
      spanParentId = Nothing
    }

prop_tracer_push_does_something :: Word64 -> Word64 -> Int -> Bool
prop_tracer_push_does_something sid tid threadid =
  let tracer0 = Tracer mempty
      sp0 = mkTestSpan sid tid
      tracer1 = tracerPushSpan tracer0 threadid sp0
   in tracer0 /= tracer1

prop_tracer_push_pop :: Word64 -> Word64 -> Int -> Bool
prop_tracer_push_pop sid tid threadid =
  let tracer0 = Tracer mempty
      sp0 = mkTestSpan sid tid
      tracer1 = tracerPushSpan tracer0 threadid sp0
      (Just sp1, tracer2) = tracerPopSpan tracer1 threadid
   in tracer0 == tracer2 && sp0 == sp1
