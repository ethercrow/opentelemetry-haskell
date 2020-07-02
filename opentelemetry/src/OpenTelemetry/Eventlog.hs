{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module OpenTelemetry.Eventlog where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS8
import Data.Unique
import Debug.Trace
import OpenTelemetry.SpanContext
import OpenTelemetry.Binary.Eventlog (SpanInFlight (..))
import Text.Printf
import Prelude hiding (span)
import OpenTelemetry.Instruments

beginSpan' :: SpanInFlight -> String -> String
beginSpan' (SpanInFlight u64) operation =
    printf "ot2 begin span %d %s" u64 operation

beginSpan :: MonadIO m => String -> m SpanInFlight
beginSpan operation = do
  u64 <- fromIntegral . hashUnique <$> liftIO newUnique
  liftIO $ traceEventIO (beginSpan' (SpanInFlight u64) operation)
  pure $ SpanInFlight u64

endSpan' :: SpanInFlight -> String
endSpan' (SpanInFlight u64) = printf "ot2 end span %d" u64

endSpan :: MonadIO m => SpanInFlight -> m ()
endSpan = liftIO . traceEventIO . endSpan'

setTag' :: SpanInFlight -> String -> BS8.ByteString -> String
setTag' (SpanInFlight u64) k v =
    printf "ot2 set tag %d %s %s" u64 k (BS8.unpack v)

setTag :: MonadIO m => SpanInFlight -> String -> BS8.ByteString -> m ()
setTag sp k v = liftIO . traceEventIO $ setTag' sp k v

addEvent' :: SpanInFlight -> String -> BS8.ByteString -> String
addEvent' (SpanInFlight u64) k v =
    printf "ot2 add event %d %s %s" u64 k (BS8.unpack v)

addEvent :: MonadIO m => SpanInFlight -> String -> BS8.ByteString -> m ()
addEvent sp k v = liftIO . traceEventIO $ addEvent' sp k v

setParentSpanContext' :: SpanInFlight -> SpanContext -> String
setParentSpanContext' (SpanInFlight u64) (SpanContext (SId sid) (TId tid)) =
    (printf "ot2 set parent %d %016x %016x" u64 tid sid)

setParentSpanContext :: MonadIO m => SpanInFlight -> SpanContext -> m ()
setParentSpanContext sp ctx = liftIO . traceEventIO $ setParentSpanContext' sp ctx

setTraceId' :: SpanInFlight -> TraceId -> String
setTraceId' (SpanInFlight u64) (TId tid) =
    printf "ot2 set traceid %d %016x" u64 tid

setTraceId :: MonadIO m => SpanInFlight -> TraceId -> m ()
setTraceId sp tid = liftIO . traceEventIO $ setTraceId' sp tid

setSpanId' :: SpanInFlight -> SpanId -> String
setSpanId' (SpanInFlight u64) (SId sid) =
    printf "ot2 set spanid %d %016x" u64 sid

setSpanId :: MonadIO m => SpanInFlight -> SpanId -> m ()
setSpanId sp sid = liftIO . traceEventIO $ setSpanId' sp sid

-- TODO: Make private
capture' ::  Instrument s a m -> Int -> String
capture' instrument v = printf "ot2 metric %s %s" (instrumentName instrument) (show v)

-- TODO: Make private
capture :: Instrument s a m -> Int -> IO ()
capture instrument v = liftIO . traceEventIO $ capture' instrument v

-- | Take a measurement for a synchronous, additive instrument ('Counter', 'UpDowncounter')
add :: Instrument 'Synchronous 'Additive m -> Int -> IO ()
add = capture

-- | Take a measurement for a synchronous, non-additive instrument ('ValueRecorder')
record :: Instrument 'Synchronous 'NonAdditive m -> Int -> IO ()
record = capture

-- | Take a measurement for an asynchronous instrument ('SumObserver', 'UpDownSumObserver', 'ValueObserver')
observe :: Instrument 'Asynchronous a m -> Int -> IO ()
observe = capture

withSpan :: forall m a. (MonadIO m, MonadMask m) => String -> (SpanInFlight -> m a) -> m a
withSpan operation action =
  fst
    <$> generalBracket
      (liftIO $ beginSpan operation)
      ( \span exitcase -> liftIO $ do
          case exitcase of
            ExitCaseSuccess _ -> pure ()
            ExitCaseException e -> do
              setTag span "error" "true"
              setTag span "error.message" (BS8.pack $ show e)
            ExitCaseAbort -> do
              setTag span "error" "true"
              setTag span "error.message" "abort"
          liftIO $ endSpan span
      )
      action

withSpan_ :: (MonadIO m, MonadMask m) => String -> m a -> m a
withSpan_ operation action = withSpan operation (const action)
