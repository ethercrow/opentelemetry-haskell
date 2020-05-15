{-# LANGUAGE OverloadedStrings #-}

module TestBinary where

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import OpenTelemetry.Binary.Eventlog
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
