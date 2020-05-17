{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module OpenTelemetry.Binary.Eventlog where

import Prelude hiding (span)

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString.Builder
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Lazy as LBS
import Data.Hashable
import Data.Unique
#if __GLASGOW_HASKELL__ >= 808
import Debug.Trace.Binary
#endif
import Data.Word (Word8, Word32, Word64)
import OpenTelemetry.SpanContext

-- This is not a Span Id in terms of OpenTelemetry.
-- It's unique only in scope of one process, not globally.
type ProcessLocalSpanSerialNumber = Word64

newtype SpanInFlight = SpanInFlight ProcessLocalSpanSerialNumber deriving (Show, Eq, Hashable)

traceBuilder :: MonadIO m => Builder -> m ()
#if __GLASGOW_HASKELL__ >= 808
traceBuilder = liftIO . traceBinaryEventIO . LBS.toStrict . toLazyByteString
#else
traceBuilder _ = return ()
#endif

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

bytes :: Int -> Int
bytes = flip shift 3

magic :: Int
magic = v .|. t .|. o
    where
      v = shift 3 $ bytes 2
      t = shift (ord 'T') $ bytes 1
      o = ord 'O'

header :: MsgType -> Builder
header (MsgType msgType) = b4 $ fromIntegral h
    where
      !h = m .|. magic
      m = shift ((fromIntegral msgType) :: Int) $ bytes 3

headerSize :: Int
headerSize = fromIntegral $ LBS.length $ toLazyByteString (header tagMsg <> b8 0)

checkSize :: Int -> m -> m
checkSize s next = do
    let exceed = s + headerSize - maxMsgLen
    if exceed > 0 then
        error $ "eventlog message size exceed 64k by " ++ show exceed
    else
        next

nextLocalSpan :: MonadIO m => m SpanInFlight
nextLocalSpan = liftIO $ (SpanInFlight . fromIntegral . hashUnique) <$> newUnique

beginSpan' :: SpanInFlight
           -> LBS.ByteString
           -> Builder
beginSpan' (SpanInFlight u) operation = do
  checkSize (fromIntegral $ LBS.length operation)
                $ header beginSpanMsg <> b8 u <> lazyByteString operation

beginSpan :: MonadIO m => LBS.ByteString -> m SpanInFlight
beginSpan operation = do
  u <- nextLocalSpan
  traceBuilder $ beginSpan' u operation
  pure u

endSpan' :: SpanInFlight -> Builder
endSpan' (SpanInFlight u) = header endSpanMsg <> b8 u

endSpan :: MonadIO m => SpanInFlight -> m ()
endSpan = traceBuilder . endSpan'

uBsBs :: MsgType
      -> SpanInFlight
      -> LBS.ByteString
      -> LBS.ByteString
      -> Builder
uBsBs msg (SpanInFlight u) k v = do
    let l = fromIntegral $ LBS.length k + 1 + LBS.length v
    checkSize l $ header msg <> b8 u <> lazyByteString k <> b1 0 <> lazyByteString v

setTag' :: SpanInFlight -> LBS.ByteString -> LBS.ByteString -> Builder
setTag' = uBsBs tagMsg

setTag :: MonadIO m => SpanInFlight -> LBS.ByteString -> LBS.ByteString -> m ()
setTag = (.) ((.) traceBuilder) . setTag'

addEvent' :: SpanInFlight -> LBS.ByteString -> LBS.ByteString -> Builder
addEvent' = uBsBs eventMsg

addEvent :: MonadIO m => SpanInFlight -> LBS.ByteString -> LBS.ByteString -> m ()
addEvent = (.) ((.) traceBuilder) . addEvent'

setParentSpanContext' :: SpanInFlight -> SpanContext -> Builder
setParentSpanContext' (SpanInFlight u) (SpanContext (SId sid) (TId tid)) =
    header setParentMsg <> b8 u <> b8 sid <> b8 tid

setParentSpanContext :: MonadIO m => SpanInFlight -> SpanContext -> m ()
setParentSpanContext = (.) traceBuilder . setParentSpanContext'

setTraceId' :: SpanInFlight -> TraceId -> Builder
setTraceId' (SpanInFlight u) (TId tid) = header setTraceMsg <> b8 u <> b8 tid

setTraceId :: MonadIO m => SpanInFlight -> TraceId -> m ()
setTraceId = (.) traceBuilder . setTraceId'

setSpanId' :: SpanInFlight -> SpanId -> Builder
setSpanId' (SpanInFlight u) (SId sid) = header setSpanMsg <> b8 u <> b8 sid

setSpanId :: MonadIO m => SpanInFlight -> SpanId -> m ()
setSpanId = (.) traceBuilder . setSpanId'

withSpan :: forall m a. (MonadIO m, MonadMask m)
            => LBS.ByteString -> (SpanInFlight -> m a) -> m a
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
