{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Eventlog where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS8
import Debug.Trace
import OpenTelemetry.SpanContext
import Text.Printf

-- TODO(divanov): replace traceEventIO with the bytestring based equivalent

beginSpan :: MonadIO m => String -> m ()
beginSpan operation = liftIO $ traceEventIO (printf "ot1 begin span %s" operation)

endSpan :: MonadIO m => m ()
endSpan = liftIO $ traceEventIO (printf "ot1 end span")

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

-- These two are supposed to be used when you have some custom control flow
-- and a given span can begin on one thread and end on another. In this case
-- the ordinary `beginSpan` and `endSpan` functions would assume a wrong thing
-- and result in

beginSpecificSpan :: TraceId -> SpanId -> String -> IO ()
beginSpecificSpan (TId tid) (SId sid) k =
  Debug.Trace.traceEventIO $
    printf "ot1 begin specific span %d %d %s" tid sid k

endSpecificSpan :: TraceId -> SpanId -> String -> IO ()
endSpecificSpan (TId tid) (SId sid) k =
  Debug.Trace.traceEventIO $
    printf "ot1 end specific span %d %d %s" tid sid k
