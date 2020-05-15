{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.ByteString.Eventlog where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Unique
import Debug.Trace.ByteString
import Data.Word (Word64)
import OpenTelemetry.SpanContext

-- This is not a Span Id in terms of OpenTelemetry.
-- It's unique only in scope of one process, not globally.
type ProcessLocalSpanSerialNumber = Word64

newtype SpanInFlight = SpanInFlight ProcessLocalSpanSerialNumber

beginSpan :: MonadIO m => LBS.ByteString -> m SpanInFlight
beginSpan operation = do
  liftIO $ do
    u64 <- fromIntegral . hashUnique <$> newUnique
    unsafeTraceEventIO . LBS.toStrict
                           . toLazyByteString
                                 $ (byteString "ot2 begin span ")
                                       <>  (word64Dec u64)
                                               <> (byteString " ")
                                                      <> (lazyByteString operation)
    return $ SpanInFlight u64


-- endSpan :: MonadIO m => SpanInFlight -> m ()
-- endSpan (SpanInFlight u64) =
--     liftIO $ LBS.toStrict joinBsLong "ot2 end span" u64 >>= unsafeTraceEventIO

-- setTag :: MonadIO m => SpanInFlight -> BS.ByteString -> BS.ByteString -> m ()
-- setTag (SpanInFlight u64) k v =
--     liftIO $ joinBsLongBsBs "ot2 set tag" u64 k v >>= unsafeTraceEventIO

-- addEvent :: MonadIO m => SpanInFlight -> BS.ByteString -> BS.ByteString -> m ()
-- addEvent (SpanInFlight u64) k v =
--     liftIO $ joinBsLongBsBs "ot2 add event" u64 k v >>= unsafeTraceEventIO

-- setParentSpanContext :: MonadIO m => SpanInFlight -> SpanContext -> m ()
-- setParentSpanContext (SpanInFlight u64) (SpanContext (SId sid) (TId tid)) =
--   liftIO $ joinBsLongLongLong "ot2 set parent" u64 tid sid >>= unsafeTraceEventIO

-- setTraceId :: MonadIO m => SpanInFlight -> TraceId -> m ()
-- setTraceId (SpanInFlight u64) (TId tid) =
--   liftIO $ joinBsLongLong "ot2 set traceid" u64 tid >>= unsafeTraceEventIO

-- setSpanId :: MonadIO m => SpanInFlight -> SpanId -> m ()
-- setSpanId (SpanInFlight u64) (SId sid) =
--   liftIO $ joinBsLongLong "ot2 set spanid" u64 sid >>= unsafeTraceEventIO

-- withSpan :: forall m a. (MonadIO m, MonadMask m) => BS.ByteString -> (SpanInFlight -> m a) -> m a
-- withSpan operation action =
--   fst
--     <$> generalBracket
--       (liftIO $ beginSpan operation)
--       ( \span exitcase -> liftIO $ do
--           case exitcase of
--             ExitCaseSuccess _ -> pure ()
--             ExitCaseException e -> do
--               setTag span "error" "true"
--               setTag span "error.message" (BS8.pack $ show e)
--             ExitCaseAbort -> do
--               setTag span "error" "true"
--               setTag span "error.message" "abort"
--           liftIO $ endSpan span
--       )
--       action

-- withSpan_ :: (MonadIO m, MonadMask m) => BS.ByteString -> m a -> m a
-- withSpan_ operation action = withSpan operation (const action)
