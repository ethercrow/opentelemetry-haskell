{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- This is an approximate implementation of https://www.w3.org/TR/trace-context

module OpenTelemetry.Propagation where

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (ord)
import Data.List (find)
import Data.String
import Data.Word
import OpenTelemetry.Common
import Text.Printf

data PropagationFormat
  = PropagationFormat
      { propagateFromHeaders :: forall key. (Semigroup key, IsString key, Eq key) => [(key, BS.ByteString)] -> Maybe SpanContext,
        propagateToHeaders :: forall key. (Semigroup key, IsString key, Eq key) => SpanContext -> [(key, BS.ByteString)]
      }

-- | (p1 <> p2) parses like p1, then p2 as a fallback. (p1 <> p2) injects like p1.
instance Semigroup PropagationFormat where
  PropagationFormat from1 to1 <> PropagationFormat from2 _to2 =
    let from headers = from1 headers <|> from2 headers
        to context = to1 context
     in PropagationFormat from to

w3cTraceContext :: PropagationFormat
w3cTraceContext = PropagationFormat from to
  where
    from headers =
      case find ((== "traceparent") . fst) headers of
        Just (_, (parseSpanContext -> mctx)) -> mctx
        _ -> Nothing
    to (SpanContext (SId sid) (TId tid)) =
      [("traceparent", BS8.pack $ printf "00-%x-%x-00" tid sid)]

b3 :: PropagationFormat
b3 = typical_opentracing_format_with_prefix "x-b3-"

otTracer :: PropagationFormat
otTracer = typical_opentracing_format_with_prefix "ot-tracer-"

typical_opentracing_format_with_prefix :: String -> PropagationFormat
typical_opentracing_format_with_prefix prefix = PropagationFormat from to
  where
    to (SpanContext (SId sid) (TId tid)) =
      [ (fromString prefix <> "traceid", encode_u64 tid),
        (fromString prefix <> "spanid", encode_u64 sid),
        (fromString prefix <> "sampled", "true")
      ]
    from headers =
      let traceidKey = fromString prefix <> "traceid"
          spanidKey = fromString prefix <> "spanid"
          go _ (Just tid, Just sid) = Just (tid, sid)
          go [] _ = Nothing
          go ((k, v) : xs) (tid, sid)
            | k == traceidKey = go xs (decode_u64 v, sid)
            | k == spanidKey = go xs (tid, decode_u64 v)
            | otherwise = go xs (tid, sid)
       in (\(t, s) -> SpanContext (SId s) (TId t)) <$> go headers (Nothing, Nothing)

parseSpanContext :: BS.ByteString -> Maybe SpanContext
parseSpanContext input =
  case BS8.split '-' input of
    ["00", (fromHex -> Just tid), (fromHex -> Just sid), _] ->
      Just $ SpanContext (SId sid) (TId tid)
    _ -> Nothing

isLowerHexDigit :: Char -> Bool
isLowerHexDigit (ord -> w) = (w >= 48 && w <= 57) || (w >= 97 && w <= 102)

fromHex :: BS.ByteString -> Maybe Word64
fromHex bytes = BS8.foldl' go (Just 0) bytes
  where
    go Nothing _ = Nothing
    go (Just !result) (ord -> d) | d >= 48 && d < 58 = Just $ result * 16 + fromIntegral d - 48
    go (Just result) (ord -> d) | d >= 97 && d < 124 = Just $ result * 16 + fromIntegral d - 87
    go _ _ = Nothing

encode_u64 :: Word64 -> BS.ByteString
encode_u64 x = BS8.pack (printf "%016x" x)

decode_u64 :: BS.ByteString -> Maybe Word64
decode_u64 bytes | BS.length bytes > 16 = Nothing
decode_u64 bytes = BS.foldl' go (Just 0) bytes
  where
    go Nothing _ = Nothing
    go (Just !result) d | d >= 48 && d < 58 = Just $ result * 16 + fromIntegral d - 48
    go (Just result) d | d >= 97 && d < 124 = Just $ result * 16 + fromIntegral d - 87
    go _ _ = Nothing
