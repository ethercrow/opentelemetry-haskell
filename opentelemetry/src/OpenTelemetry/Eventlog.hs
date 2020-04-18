{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Eventlog where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS8
import Debug.Trace
import OpenTelemetry.SpanContext
import Text.Printf

foreign import ccall "flushTrace" flushTrace :: IO ()

-- TODO(divanov): replace traceEventIO with the bytestring based equivalent

beginSpan :: MonadIO m => String -> m ()
beginSpan operation = liftIO $ traceEventIO (printf "ot1 begin span %s" operation)

endSpan :: MonadIO m => m ()
endSpan = liftIO $ do
  traceEventIO (printf "ot1 end span")
  flushTrace

setTag :: MonadIO m => String -> BS8.ByteString -> m ()
setTag k v = liftIO $ traceEventIO (printf "ot1 set tag %s %s" k (BS8.unpack v))

addEvent :: MonadIO m => String -> BS8.ByteString -> m ()
addEvent k v = liftIO $ traceEventIO (printf "ot1 add event %s %s" k (BS8.unpack v))

setParentSpanContext :: MonadIO m => SpanContext -> m ()
setParentSpanContext (SpanContext (SId sid) (TId tid)) =
  liftIO $ traceEventIO (printf "ot1 set parent %016x %016x" tid sid)

setTraceId :: MonadIO m => TraceId -> m ()
setTraceId (TId tid) =
  liftIO $ traceEventIO (printf "ot1 set traceid %016x" tid)

setSpanId :: MonadIO m => SpanId -> m ()
setSpanId (SId sid) =
  liftIO $ traceEventIO (printf "ot1 set spanid %016x" sid)

withSpan :: forall m a. (MonadIO m, MonadMask m) => String -> m a -> m a
withSpan operation action =
  fst
    <$> generalBracket
      (liftIO $ beginSpan operation)
      ( \_span exitcase -> liftIO $ do
          case exitcase of
            ExitCaseSuccess _ -> pure ()
            ExitCaseException e -> do
              setTag "error" "true"
              setTag "error.message" (BS8.pack $ show e)
            ExitCaseAbort -> do
              setTag "error" "true"
              setTag "error.message" "abort"
          liftIO endSpan
      )
      (\_span -> action)
