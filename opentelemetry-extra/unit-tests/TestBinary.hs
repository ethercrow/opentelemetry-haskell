{-# LANGUAGE OverloadedStrings #-}

module TestBinary where

import Arbitrary ()
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import LogEventSerializer
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Eventlog_Internal
import Test.Tasty.QuickCheck

newtype MsgTypeAr = MsgTypeAr MsgType deriving (Show)

instance Arbitrary MsgTypeAr where
  arbitrary = elements $ map (MsgTypeAr . MsgType) [1 .. 100]

prop_header_layout_prefix_ot3 :: MsgTypeAr -> Property
prop_header_layout_prefix_ot3 (MsgTypeAr msgType) =
  LBS.take 3 (toLazyByteString (header msgType)) === "OT\x03"

prop_header_layout_suffix_msg :: MsgTypeAr -> Property
prop_header_layout_suffix_msg (MsgTypeAr msgType@(MsgType msgTypeId)) =
  LBS.last (toLazyByteString (header msgType)) === msgTypeId

parseRight :: BS.ByteString -> OpenTelemetryEventlogEvent
parseRight bs = case parseByteString bs of
  Nothing -> error "No event"
  Just ev -> ev

prop_binary_roundtrip :: OpenTelemetryEventlogEvent -> Property
prop_binary_roundtrip a = a === parseRight (logEventToBs a)
