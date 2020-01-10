module OpenTelemetry.Implicit where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Text as T
import OpenTelemetry.Common
import System.IO.Unsafe

withSpan :: (MonadIO m, MonadCatch m) => T.Text -> m a -> m a
withSpan operation action = error "withSpan: not implemented"

setTag :: forall value m. Monad m => T.Text -> value -> m ()
setTag k v = error "setTag: not implemented"

addEvent :: forall m. Monad m => T.Text -> m ()
addEvent name = error "addEvent: not implemented"

withOpenTelemetry :: (MonadIO m, MonadMask m) => OpenTelemetryConfig -> m a -> m a
withOpenTelemetry OpenTelemetryConfig {..} action = action `finally` do
  liftIO $ shutdown otcSpanExporter

setImplicitTracer :: (MonadIO m, MonadCatch m) => Tracer -> m ()
setImplicitTracer tracer = liftIO $ putMVar globalSharedMutableState tracer

getTracer :: IO Tracer
getTracer = readMVar globalSharedMutableState

globalSharedMutableState :: MVar Tracer
globalSharedMutableState = unsafePerformIO newEmptyMVar
{-# NOINLINE globalSharedMutableState #-}
