{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Binary.Parser where

import qualified Codec.ByteString.Parser as CSP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Bits
import Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
import OpenTelemetry.Binary.Eventlog (SpanInFlight (..), MsgType (..), magic)
import OpenTelemetry.Common
import OpenTelemetry.Handler
import qualified OpenTelemetry.Parser as P
import OpenTelemetry.SpanContext

headerP :: CSP.Parser (Maybe MsgType)
headerP = do
  h <- CSP.getWord32le
  let !msgTypeId = shiftR h 24
  if magic == fromIntegral h .&.  magic then
      if msgTypeId > 7 && msgTypeId < 1
      then fail $ "Bad Msg Type: " ++ show msgTypeId
      else return . Just . MsgType . fromIntegral $ msgTypeId
  else
      return Nothing

b8P :: CSP.Parser Word64
b8P = CSP.getWord64le

lazyBs2Txt :: LBS.ByteString -> T.Text
lazyBs2Txt = TE.decodeUtf8 . LBS.toStrict

lastStringP :: CSP.Parser T.Text
lastStringP = lazyBs2Txt <$> CSP.getRemainingLazyByteString

cStringP :: CSP.Parser T.Text
cStringP = lazyBs2Txt <$> CSP.getLazyByteStringNul

logEventBodyP :: MsgType -> CSP.Parser LogEvent
logEventBodyP msgType =
  case msgType of
    MsgType 1 -> BeginSpanEv <$> (SpanInFlight <$> b8P)
                 <*> lastStringP
    MsgType 2 -> EndSpanEv <$> (SpanInFlight <$> b8P)
    MsgType 3 -> TagEv <$> (SpanInFlight <$> b8P)
                 <*> cStringP <*> lastStringP
    MsgType 4 -> EventEv <$> (SpanInFlight <$> b8P)
                 <*> cStringP <*> lastStringP
    MsgType 5 -> SetParentEv <$> (SpanInFlight <$> b8P)
                 <*> (SpanContext <$> (SId <$> b8P) <*> (TId <$> b8P))
    MsgType 6 -> SetTraceEv <$> (SpanInFlight <$> b8P)
                 <*> (TId <$> b8P)
    MsgType 7 -> SetSpanEv <$> (SpanInFlight <$> b8P)
                 <*> (SId <$> b8P)
    MsgType mti ->
        fail $ "Log event of type " ++ show mti ++ " is not supported"

logEventP :: CSP.Parser (Maybe LogEvent)
logEventP =
  CSP.lookAheadM headerP >>= \case
     Nothing -> return Nothing
     Just msgType -> logEventBodyP msgType >>= return . Just

parse :: BS.ByteString
      -> Either String (Maybe LogEvent)
parse = CSP.runParser logEventP . LBS.fromStrict
