{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Binary.Parser where

import qualified Data.Binary.Get as DBG
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Bits
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
import OpenTelemetry.Binary.Eventlog (SpanInFlight (..), MsgType (..), magic)
import OpenTelemetry.Common
import OpenTelemetry.Handler
import OpenTelemetry.SpanContext

headerP :: DBG.Get (Maybe MsgType)
headerP = do
  h <- DBG.getWord32le
  let !msgTypeId = shiftR h 24
  if magic == fromIntegral h .&.  magic then
      if msgTypeId > 7 && msgTypeId < 1
      then fail $ "Bad Msg Type: " ++ show msgTypeId
      else return . Just . MsgType . fromIntegral $ msgTypeId
  else
      return Nothing

b8P :: DBG.Get Word64
b8P = DBG.getWord64le

lazyBs2Txt :: LBS.ByteString -> T.Text
lazyBs2Txt = TE.decodeUtf8 . LBS.toStrict

lastStringP :: DBG.Get T.Text
lastStringP = lazyBs2Txt <$> DBG.getRemainingLazyByteString

cStringP :: DBG.Get T.Text
cStringP = lazyBs2Txt <$> DBG.getLazyByteStringNul

logEventBodyP :: MsgType -> DBG.Get LogEvent
logEventBodyP msgType =
  case msgType of
    MsgType 1 -> BeginSpanEv <$> (SpanInFlight <$> b8P)
                 <*> (SpanName <$> lastStringP)
    MsgType 2 -> EndSpanEv <$> (SpanInFlight <$> b8P)
    MsgType 3 -> TagEv <$> (SpanInFlight <$> b8P)
                 <*> (TagName <$> cStringP) <*> (TagVal <$> lastStringP)
    MsgType 4 -> EventEv <$> (SpanInFlight <$> b8P)
                 <*> (EventName <$> cStringP) <*> (EventVal <$> lastStringP)
    MsgType 5 -> SetParentEv <$> (SpanInFlight <$> b8P)
                 <*> (SpanContext <$> (SId <$> b8P) <*> (TId <$> b8P))
    MsgType 6 -> SetTraceEv <$> (SpanInFlight <$> b8P)
                 <*> (TId <$> b8P)
    MsgType 7 -> SetSpanEv <$> (SpanInFlight <$> b8P)
                 <*> (SId <$> b8P)
    MsgType mti ->
        fail $ "Log event of type " ++ show mti ++ " is not supported"

logEventP :: DBG.Get (Maybe LogEvent)
logEventP =
  DBG.lookAheadM headerP >>= \case
     Nothing -> return Nothing
     Just msgType -> logEventBodyP msgType >>= return . Just

parse :: BS.ByteString
      -> Maybe LogEvent
parse = DBG.runGet logEventP . LBS.fromStrict
