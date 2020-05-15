{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Binary.Eventlog where

import Prelude hiding (span)

import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString.Builder
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Lazy as LBS
import Data.Unique
import Debug.Trace.Binary
import Data.Word (Word8, Word32, Word64)
import OpenTelemetry.SpanContext

-- This is not a Span Id in terms of OpenTelemetry.
-- It's unique only in scope of one process, not globally.
type ProcessLocalSpanSerialNumber = Word64

newtype SpanInFlight = SpanInFlight ProcessLocalSpanSerialNumber

traceBuilder :: MonadIO m => Builder -> m ()
traceBuilder = liftIO . traceBinaryEventIO . LBS.toStrict . toLazyByteString

newtype MsgType = MsgType Word8 deriving (Show)

beginSpanMsg :: MsgType
beginSpanMsg = MsgType 1
endSpanMsg :: MsgType
endSpanMsg = MsgType 2
tagMsg :: MsgType
tagMsg = MsgType 3
eventMsg :: MsgType
eventMsg = MsgType 4
setParentMsg :: MsgType
setParentMsg = MsgType 5
setTraceMsg :: MsgType
setTraceMsg = MsgType 6
setSpanMsg :: MsgType
setSpanMsg = MsgType 7

b1 :: Word8 -> Builder
b1 = word8
b4 :: Word32 -> Builder
b4 = word32LE
b8 :: Word64 -> Builder
b8 = word64LE

maxMsgLen :: Int
maxMsgLen = shift 2 16

header :: MsgType -> Builder
header (MsgType msgType) = b4 $ fromIntegral h
    where
      !h = m .|. v .|. t .|. o
      m = shift ((fromIntegral msgType) :: Int) $ bytes 3
      v = shift 3 $ bytes 2
      t = shift (ord 'T') $ bytes 1
      o = ord 'O'
      bytes = flip shift 3

headerSize :: Int
headerSize = fromIntegral $ LBS.length $ toLazyByteString (header tagMsg <> b8 0)

checkSize :: MonadIO m => Int -> m () -> m ()
checkSize s next = do
    let !exceed = s + headerSize - maxMsgLen
    if exceed > 0 then
        liftIO $ throwIO $ AssertionFailed
                   $ "eventlog message size exceed 64k by " ++ show exceed
    else
        next


beginSpan :: MonadIO m => LBS.ByteString -> m SpanInFlight
beginSpan operation = do
  u <- liftIO $ fromIntegral . hashUnique <$> newUnique
  checkSize (fromIntegral $ LBS.length operation) $ traceBuilder
                $ header beginSpanMsg <> b8 u <> lazyByteString operation
  pure $ SpanInFlight u


endSpan :: MonadIO m => SpanInFlight -> m ()
endSpan (SpanInFlight u) =
    traceBuilder $ header endSpanMsg <> b8 u

uBsBs :: MonadIO m => MsgType -> SpanInFlight -> LBS.ByteString -> LBS.ByteString -> m ()
uBsBs msg (SpanInFlight u) k v = do
    let l = fromIntegral $ LBS.length k + 1 + LBS.length v
    checkSize l $ traceBuilder $ header msg <> b8 u
                  <> lazyByteString k <> b1 0
                         <> lazyByteString v

setTag :: MonadIO m => SpanInFlight -> LBS.ByteString -> LBS.ByteString -> m ()
setTag = uBsBs tagMsg

addEvent :: MonadIO m => SpanInFlight -> LBS.ByteString -> LBS.ByteString -> m ()
addEvent =  uBsBs eventMsg

setParentSpanContext :: MonadIO m => SpanInFlight -> SpanContext -> m ()
setParentSpanContext (SpanInFlight u) (SpanContext (SId sid) (TId tid)) =
    traceBuilder $ header setParentMsg <> b8 u <> b8 tid <> b8 sid

setTraceId :: MonadIO m => SpanInFlight -> TraceId -> m ()
setTraceId (SpanInFlight u) (TId tid) =
  traceBuilder $ header setTraceMsg <> b8 u <> b8 tid

setSpanId :: MonadIO m => SpanInFlight -> SpanId -> m ()
setSpanId (SpanInFlight u) (SId sid) =
  traceBuilder $ header setSpanMsg <> b8 u <> b8 sid

withSpan :: forall m a. (MonadIO m, MonadMask m) => LBS.ByteString -> (SpanInFlight -> m a) -> m a
withSpan operation action =
  fst
    <$> generalBracket
      (liftIO $ beginSpan operation)
      ( \span exitcase -> liftIO $ do
          case exitcase of
            ExitCaseSuccess _ -> pure ()
            ExitCaseException e -> do
              setTag span "error" "true"
              setTag span "error.message" (LBS8.pack $ take maxMsgLen $ show e)
            ExitCaseAbort -> do
              setTag span "error" "true"
              setTag span "error.message" "abort"
          liftIO $ endSpan span
      )
      action

withSpan_ :: (MonadIO m, MonadMask m) => LBS.ByteString -> m a -> m a
withSpan_ operation action = withSpan operation (const action)
