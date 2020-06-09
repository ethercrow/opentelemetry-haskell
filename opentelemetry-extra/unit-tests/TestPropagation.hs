{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TestPropagation where

import qualified Data.ByteString.Char8 as BS
import Data.Word
import OpenTelemetry.Propagation
import OpenTelemetry.SpanContext
import Test.Tasty.HUnit
import Text.Printf

prop_render_parse_roundtrip_w3c :: Word64 -> Word64 -> Bool
prop_render_parse_roundtrip_w3c sid tid =
  let c = SpanContext (SId sid) (TId tid)
   in Just c == propagateFromHeaders w3cTraceContext @String (propagateToHeaders w3cTraceContext c)

prop_render_parse_roundtrip_b3 :: Word64 -> Word64 -> Bool
prop_render_parse_roundtrip_b3 sid tid =
  let c = SpanContext (SId sid) (TId tid)
   in Just c == propagateFromHeaders b3 @String (propagateToHeaders b3 c)

unit_render_smoke :: Assertion
unit_render_smoke =
  propagateToHeaders w3cTraceContext (SpanContext (SId 1) (TId 2)) @?= expected
      where
        expected = [("traceparent" :: String, "00-2-1-00")]

unit_parse_smoke :: Assertion
unit_parse_smoke =
  propagateFromHeaders w3cTraceContext @String [("traceparent", "00-2-1-00")] @?= Just (SpanContext (SId 1) (TId 2))

prop_fromHex :: Word64 -> Bool
prop_fromHex x =
  Just x == fromHex (BS.pack $ printf "%x" x)

unit_fromHex_smoke :: Assertion
unit_fromHex_smoke =
  Just 1 @?= fromHex "1"
