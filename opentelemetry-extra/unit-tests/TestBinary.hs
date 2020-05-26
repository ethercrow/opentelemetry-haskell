{-# LANGUAGE OverloadedStrings #-}

module TestBinary where

import Arbitrary ()
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import LogEventSerializer
import OpenTelemetry.Binary.Eventlog
import OpenTelemetry.EventlogStreaming_Internal

import Test.QuickCheck

newtype MsgTypeAr = MsgTypeAr MsgType deriving (Show)

instance Arbitrary MsgTypeAr where
    arbitrary = elements $ map (MsgTypeAr . MsgType) [1..100]

prop_header_layout_prefix_ot3 :: MsgTypeAr -> Bool
prop_header_layout_prefix_ot3 (MsgTypeAr msgType) =
  LBS.take 3 (toLazyByteString (header msgType)) == "OT\03"

prop_header_layout_suffix_msg :: MsgTypeAr -> Bool
prop_header_layout_suffix_msg (MsgTypeAr msgType@(MsgType msgTypeId)) =
  LBS.last (toLazyByteString (header msgType)) == msgTypeId

parseRight :: BS.ByteString -> LogEvent
parseRight bs = case parseByteString bs of
                  Nothing -> error "No event"
                  Just ev -> ev

prop_binary_marshaling :: LogEvent -> Bool
prop_binary_marshaling a = a == parseRight (logEventToBs a)
