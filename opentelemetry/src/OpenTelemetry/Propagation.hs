{-# LANGUAGE OverloadedStrings #-}

-- This is an approximate implementation of https://www.w3.org/TR/trace-context

module OpenTelemetry.Propagation where

import Data.Word
import qualified Data.ByteString.Char8 as BS
import Data.Char (ord)
import Data.List (find)
import Data.String
import GHC.Generics
import OpenTelemetry.Common
import Text.Printf

data TraceParent = TraceParent Int Int
  deriving (Eq, Show, Generic)

extractSpanContextFromHeaders :: (IsString key, Eq key) => [(key, BS.ByteString)] -> Maybe SpanContext
extractSpanContextFromHeaders headers =
  case find ((== "traceparent") . fst) headers of
    Just (_, value) -> case parseSpanContext value of
      Just c -> Just c
    _ -> Nothing

parseSpanContext :: BS.ByteString -> Maybe SpanContext
parseSpanContext input =
  case BS.split '-' input of
    ["00", (fromHex -> Just tid), (fromHex -> Just sid), _] ->
      Just $ SpanContext (SId sid) (TId tid)
    _ -> Nothing

renderSpanContext :: SpanContext -> BS.ByteString
renderSpanContext (SpanContext (SId sid) (TId tid)) =
  BS.pack $ printf "00-%x-%x-00" tid sid

isLowerHexDigit :: Char -> Bool
isLowerHexDigit (ord -> w) = (w >= 48 && w <= 57) || (w >= 97 && w <= 102)

fromHex :: BS.ByteString -> Maybe Word64
fromHex bytes = BS.foldl' go (Just 0) bytes
  where
    go Nothing _ = Nothing
    go (Just !result) (ord -> d) | d >= 48 && d < 58 = Just $ result * 16 + fromIntegral d - 48
    go (Just result) (ord -> d) | d >= 97 && d < 124 = Just $ result * 16 + fromIntegral d - 87
    go _ _ = Nothing

data PropagationFormat = W3CTraceContext

inject :: PropagationFormat -> SpanContext -> [(String, BS.ByteString)]
inject W3CTraceContext ctx = [("traceparent", renderSpanContext ctx)]
