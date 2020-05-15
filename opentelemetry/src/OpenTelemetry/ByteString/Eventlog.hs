{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.ByteString.Eventlog where

import Prelude hiding (span)

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Lazy as LBS
import Data.Unique
import Debug.Trace.ByteString
import Data.Word (Word64)
import OpenTelemetry.SpanContext

-- This is not a Span Id in terms of OpenTelemetry.
-- It's unique only in scope of one process, not globally.
type ProcessLocalSpanSerialNumber = Word64

newtype SpanInFlight = SpanInFlight ProcessLocalSpanSerialNumber

traceBuilder :: MonadIO m => Builder -> m ()
traceBuilder = liftIO . unsafeTraceEventIO . LBS.toStrict . toLazyByteString

beginSpan :: MonadIO m => LBS.ByteString -> m SpanInFlight
beginSpan operation = do
  u64 <- liftIO $ fromIntegral . hashUnique <$> newUnique
  traceBuilder $ (byteString "ot2 begin span ")
                   <>  (word64Dec u64)
                           <> (char8 ' ')
                                  <> (lazyByteString operation)
  pure $ SpanInFlight u64


endSpan :: MonadIO m => SpanInFlight -> m ()
endSpan (SpanInFlight u64) =
    traceBuilder $ byteString "ot2 end span " <> word64Dec u64

setTag :: MonadIO m => SpanInFlight -> LBS.ByteString -> LBS.ByteString -> m ()
setTag (SpanInFlight u64) k v =
    traceBuilder $ byteString "ot2 set tag "
                     <> word64Dec u64
                            <> char8 ' '
                                   <> lazyByteString k
                                      <> char8 ' '
                                          <>  lazyByteString v

addEvent :: MonadIO m => SpanInFlight -> LBS.ByteString -> LBS.ByteString -> m ()
addEvent (SpanInFlight u64) k v =
    traceBuilder $ byteString "ot2 add event "
                     <> word64Dec u64
                        <> char8 ' '
                           <> lazyByteString k
                              <> lazyByteString v

setParentSpanContext :: MonadIO m => SpanInFlight -> SpanContext -> m ()
setParentSpanContext (SpanInFlight u64) (SpanContext (SId sid) (TId tid)) =
    traceBuilder $ byteString  "ot2 set parent "
                     <> word64Dec u64
                        <> byteString " 0x"
                           <> word64HexFixed tid
                              <> byteString " 0x"
                                 <> word64HexFixed sid

setTraceId :: MonadIO m => SpanInFlight -> TraceId -> m ()
setTraceId (SpanInFlight u64) (TId tid) =
  traceBuilder $ byteString "ot2 set traceid "
               <> word64Dec u64
                  <> byteString " 0x"
                     <> word64HexFixed tid

setSpanId :: MonadIO m => SpanInFlight -> SpanId -> m ()
setSpanId (SpanInFlight u64) (SId sid) =
  traceBuilder $ byteString "ot2 set spanid "
                   <> word64Dec u64
                      <> byteString " 0x"
                         <> word64HexFixed sid

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
              setTag span "error.message" (LBS8.pack $ show e)
            ExitCaseAbort -> do
              setTag span "error" "true"
              setTag span "error.message" "abort"
          liftIO $ endSpan span
      )
      action

withSpan_ :: (MonadIO m, MonadMask m) => LBS.ByteString -> m a -> m a
withSpan_ operation action = withSpan operation (const action)
