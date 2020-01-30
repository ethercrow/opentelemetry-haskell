{-# LANGUAGE OverloadedStrings #-}

module TestPropagation where

import qualified Data.ByteString.Char8 as BS
import Data.Word
import OpenTelemetry.Common
import OpenTelemetry.Propagation
import Test.Tasty.HUnit
import Text.Printf

prop_render_parse_roundtrip :: Word64 -> Word64 -> Bool
prop_render_parse_roundtrip sid tid =
  let c = SpanContext (SId sid) (TId tid)
   in Just c == extractSpanContextFromHeaders [("traceparent", renderSpanContext c)]

unit_render_smoke =
  renderSpanContext (SpanContext (SId 1) (TId 2)) @?= "00-2-1-00"

unit_parse_smoke =
  extractSpanContextFromHeaders [("traceparent", "00-2-1-00")] @?= Just (SpanContext (SId 1) (TId 2))

prop_fromHex :: Word64 -> Bool
prop_fromHex x =
  Just x == fromHex (BS.pack $ printf "%x" x)

unit_fromHex_smoke =
  Just 1 @?= fromHex "1"
