{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Debug.Trace.Binary
import Data.Word (Word8, Word32, Word64)
import OpenTelemetry.SpanContext

-- This is not a Span Id in terms of OpenTelemetry.
-- It's unique only in scope of one process, not globally.
type ProcessLocalSpanSerialNumber = Word64

newtype SpanInFlight = SpanInFlight ProcessLocalSpanSerialNumber deriving (Show, Eq, Hashable)

traceBuilder :: MonadIO m => Builder -> m ()
traceBuilder = liftIO . traceBinaryEventIO . LBS.toStrict . toLazyByteString

newtype MsgType = MsgType Word8 deriving (Show)

{-# INLINE beginSpanMsg #-}
beginSpanMsg :: MsgType
beginSpanMsg = MsgType 1

{-# INLINE endSpanMsg #-}
endSpanMsg :: MsgType
endSpanMsg = MsgType 2

{-# INLINE tagMsg #-}
tagMsg :: MsgType
tagMsg = MsgType 3

{-# INLINE eventMsg #-}
eventMsg :: MsgType
eventMsg = MsgType 4

{-# INLINE setParentMsg #-}
setParentMsg :: MsgType
setParentMsg = MsgType 5

{-# INLINE setTraceMsg #-}
setTraceMsg :: MsgType
setTraceMsg = MsgType 6

{-# INLINE setSpanMsg #-}
setSpanMsg :: MsgType
setSpanMsg = MsgType 7

{-# INLINE b1 #-}
b1 :: Word8 -> Builder
b1 = word8

{-# INLINE b4 #-}
b4 :: Word32 -> Builder
b4 = word32LE

{-# INLINE b8 #-}
b8 :: Word64 -> Builder
b8 = word64LE

{-# INLINE maxMsgLen #-}
maxMsgLen :: Int
maxMsgLen = shift 2 16

{-# INLINE bytes #-}
bytes :: Int -> Int
bytes = flip shift 3

{-# INLINE magic #-}
magic :: Int
magic = v .|. t .|. o
    where
      !v = shift 3 $ bytes 2
      !t = shift (ord 'T') $ bytes 1
      !o = ord 'O'


{-# INLINE header #-}
header :: MsgType -> Builder
header (MsgType msgType) = b4 $ fromIntegral h
    where
      !h = m .|. magic
      !m = shift ((fromIntegral msgType) :: Int) $ bytes 3

{-# INLINE headerSize #-}
headerSize :: Int
headerSize = fromIntegral $ LBS.length $ toLazyByteString (header tagMsg <> b8 0)

{-# INLINE checkSize #-}
checkSize :: Int -> m -> m
checkSize s next = do
    let !exceed = s + headerSize - maxMsgLen
    if exceed > 0 then
        error $ "eventlog message size exceed 64k by " ++ show exceed
    else
        next

{-# INLINE nextLocalSpan #-}
nextLocalSpan :: MonadIO m => m SpanInFlight
nextLocalSpan = liftIO $ (SpanInFlight . fromIntegral . hashUnique) <$> newUnique

{-# INLINE beginSpan' #-}
beginSpan' :: SpanInFlight
           -> LBS.ByteString
           -> Builder
beginSpan' (SpanInFlight u) operation = do
  checkSize (fromIntegral $ LBS.length operation)
                $ header beginSpanMsg <> b8 u <> lazyByteString operation

{-# INLINE beginSpan #-}
beginSpan :: MonadIO m => LBS.ByteString -> m SpanInFlight
beginSpan operation = do
  u <- nextLocalSpan
  traceBuilder $ beginSpan' u operation
  pure u

{-# INLINE endSpan' #-}
endSpan' :: SpanInFlight -> Builder
endSpan' (SpanInFlight u) = header endSpanMsg <> b8 u

{-# INLINE endSpan #-}
endSpan :: MonadIO m => SpanInFlight -> m ()
endSpan = traceBuilder . endSpan'

{-# INLINE uBsBs #-}
uBsBs :: MsgType
      -> SpanInFlight
      -> LBS.ByteString
      -> LBS.ByteString
      -> Builder
uBsBs msg (SpanInFlight u) k v = do
    let !l = fromIntegral $ LBS.length k + 1 + LBS.length v
    checkSize l $ header msg <> b8 u <> lazyByteString k <> b1 0 <> lazyByteString v

{-# INLINE setTag' #-}
setTag' :: SpanInFlight -> LBS.ByteString -> LBS.ByteString -> Builder
setTag' = uBsBs tagMsg

{-# INLINE setTag #-}
setTag :: MonadIO m => SpanInFlight -> LBS.ByteString -> LBS.ByteString -> m ()
setTag sp k v = traceBuilder $ setTag' sp k v

{-# INLINE addEvent' #-}
addEvent' :: SpanInFlight -> LBS.ByteString -> LBS.ByteString -> Builder
addEvent' = uBsBs eventMsg

{-# INLINE addEvent #-}
addEvent :: MonadIO m => SpanInFlight -> LBS.ByteString -> LBS.ByteString -> m ()
addEvent sp k v = traceBuilder $ addEvent' sp k v

setParentSpanContext' :: SpanInFlight -> SpanContext -> Builder
setParentSpanContext' (SpanInFlight u) (SpanContext (SId sid) (TId tid)) =
    header setParentMsg <> b8 u <> b8 sid <> b8 tid

setParentSpanContext :: MonadIO m => SpanInFlight -> SpanContext -> m ()
setParentSpanContext sp ctx = traceBuilder $ setParentSpanContext' sp ctx

{-# INLINE setTraceId' #-}
setTraceId' :: SpanInFlight -> TraceId -> Builder
setTraceId' (SpanInFlight u) (TId tid) = header setTraceMsg <> b8 u <> b8 tid

{-# INLINE setTraceId #-}
setTraceId :: MonadIO m => SpanInFlight -> TraceId -> m ()
setTraceId sp tid = traceBuilder $ setTraceId' sp tid

{-# INLINE setSpanId' #-}
setSpanId' :: SpanInFlight -> SpanId -> Builder
setSpanId' (SpanInFlight u) (SId sid) = header setSpanMsg <> b8 u <> b8 sid

{-# INLINE setSpanId #-}
setSpanId :: MonadIO m => SpanInFlight -> SpanId -> m ()
setSpanId sp sid = traceBuilder $ setSpanId' sp sid

{-# INLINE withSpan #-}
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
