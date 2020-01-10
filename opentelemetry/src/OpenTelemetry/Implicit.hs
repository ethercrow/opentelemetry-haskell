module OpenTelemetry.Implicit where

import GHC.Conc
import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Text as T
import OpenTelemetry.Common
import OpenTelemetry.Explicit
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HM 
import System.IO.Unsafe
import System.Random

withSpan :: (MonadIO m, MonadMask m) => T.Text -> m a -> m a
withSpan operation action = do
  tid <- liftIO myThreadId
  sid <- liftIO randomIO
  timestamp <- liftIO now64
  bracket
    ( liftIO $ modifyMVar_ globalSharedMutableState $ \GlobalSharedMutableState {..} -> do
        let !ctx = case HM.lookup tid (tracerSpanStacks gTracer) of
              Nothing -> SpanContext (SId sid) (TId sid)
              Just ((spanContext -> SpanContext _ tid) NE.:| _) -> SpanContext (SId sid) tid
            !sp = Span ctx operation timestamp 0 OK
            !tracer = tracerPushSpan gTracer tid sp
        pure $! GlobalSharedMutableState gSpanExporter tracer
    )
    (\_ -> pure ())
    (\_ -> action)

setTag :: forall value m. MonadIO m => T.Text -> value -> m ()
setTag k v = do
  tid <- liftIO myThreadId
  error "setTag: not implemented"

addEvent :: forall m. MonadIO m => T.Text -> m ()
addEvent name = do
  tid <- liftIO myThreadId
  error "addEvent: not implemented"

withOpenTelemetry :: (MonadIO m, MonadMask m) => OpenTelemetryConfig -> m a -> m a
withOpenTelemetry OpenTelemetryConfig {..} action =
  bracket
    ( liftIO $ do
        putMVar globalSharedMutableState (GlobalSharedMutableState otcSpanExporter defaultTracer)
        pure ()
    )
    (\_ -> liftIO $ shutdown otcSpanExporter)
    (\_ -> action)

data GlobalSharedMutableState
  = GlobalSharedMutableState
      { gSpanExporter :: Exporter Span,
        gTracer :: Tracer ThreadId
      }

defaultTracer :: Tracer ThreadId
defaultTracer = Tracer mempty

globalSharedMutableState :: MVar GlobalSharedMutableState
globalSharedMutableState = unsafePerformIO newEmptyMVar
{-# NOINLINE globalSharedMutableState #-}
