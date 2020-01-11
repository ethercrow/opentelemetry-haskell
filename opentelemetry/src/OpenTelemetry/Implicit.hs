module OpenTelemetry.Implicit where

import Data.Maybe
import GHC.Conc
import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Text as T
import OpenTelemetry.Common
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.HashMap.Strict as HM 
import System.IO
import System.IO.Unsafe
import System.Random

withSpan :: (MonadIO m, MonadMask m) => T.Text -> m a -> m a
withSpan operation action = do
  threadId <- liftIO myThreadId
  sid <- liftIO randomIO
  startedAt <- liftIO now64
  bracket
    ( liftIO $ modifyMVar_ globalSharedMutableState $ \GlobalSharedMutableState {..} -> do
        let !ctx = case HM.lookup threadId (tracerSpanStacks gTracer) of
              Nothing -> SpanContext (SId sid) (TId sid)
              Just ((spanContext -> SpanContext _ tid) :| _) -> SpanContext (SId sid) tid
            !sp = Span ctx operation startedAt 0 mempty OK
            !tracer = tracerPushSpan gTracer threadId sp
        pure $! GlobalSharedMutableState gSpanExporter tracer
    )
    (\_ -> do
      liftIO $ modifyMVar_ globalSharedMutableState $ \GlobalSharedMutableState {..} -> do
        let (mspan, tracer) = tracerPopSpan gTracer threadId
        case mspan of
          Nothing -> pure ()
          Just sp -> do
            finishedAt <- liftIO now64
            res <- export gSpanExporter [sp { spanFinishedAt = finishedAt }]
            case res of
              ExportSuccess -> pure ()
              _ -> error $ "exporting span failed: " <> show sp
        pure $! GlobalSharedMutableState gSpanExporter tracer)
    (\_ -> action) -- TODO(divanov): set error=true on exception

setTag :: forall value m. (MonadIO m, ToTagValue value) => T.Text -> value -> m ()
setTag k v = modifyCurrentSpan (\sp ->
  sp { spanTags = HM.insert k (toTagValue v) (spanTags sp)})

addEvent :: forall m. MonadIO m => T.Text -> m ()
addEvent name = do
  tid <- liftIO myThreadId
  error "addEvent: not implemented"

withOpenTelemetry :: (MonadIO m, MonadMask m) => OpenTelemetryConfig -> m a -> m a
withOpenTelemetry OpenTelemetryConfig {..} action =
  bracket
    ( liftIO $ do
        tracer <- createTracer
        putMVar globalSharedMutableState (GlobalSharedMutableState otcSpanExporter tracer)
        pure ()
    )
    (\_ -> liftIO $ shutdown otcSpanExporter)
    (\_ -> action)

data GlobalSharedMutableState
  = GlobalSharedMutableState
      { gSpanExporter :: !(Exporter Span),
        gTracer :: !(Tracer ThreadId)
      }

getCurrentActiveSpan :: MonadIO m => m Span
getCurrentActiveSpan = do
  tid <- liftIO myThreadId
  GlobalSharedMutableState {..} <- liftIO $ readMVar globalSharedMutableState
  pure $ fromMaybe emptySpan $ tracerGetCurrentActiveSpan gTracer tid

modifyCurrentSpan :: MonadIO m => (Span -> Span) -> m ()
modifyCurrentSpan f = liftIO $ do
  tid <- myThreadId
  modifyMVar_
    globalSharedMutableState
    ( \g@(GlobalSharedMutableState {..}) ->
        case HM.lookup tid (tracerSpanStacks gTracer) of
          Nothing -> pure g
          Just (sp :| sps) ->
            let !stacks = HM.insert tid (f sp :| sps) (tracerSpanStacks gTracer)
            in pure $! g { gTracer = gTracer { tracerSpanStacks = stacks }}
    )

withChildSpanOf :: (MonadIO m, MonadMask m) => Span -> T.Text -> m a -> m a
withChildSpanOf parent operation action = do
  threadId <- liftIO myThreadId
  sid <- liftIO randomIO
  timestamp <- liftIO now64
  bracket
    ( liftIO $ modifyMVar_ globalSharedMutableState $ \GlobalSharedMutableState {..} -> do
        let threadId' = fromMaybe threadId $ HM.lookup (spanTraceId parent) (trace2thread gTracer)
        let !ctx = case HM.lookup threadId' (tracerSpanStacks gTracer) of
              Nothing -> SpanContext (SId sid) (TId sid)
              Just ((spanContext -> SpanContext _ tid) NE.:| _) -> SpanContext (SId sid) tid
            !sp = Span ctx operation timestamp 0 mempty OK
            !tracer = tracerPushSpan gTracer threadId sp
        pure $! GlobalSharedMutableState gSpanExporter tracer
    )
    (\_ -> pure ())
    (\_ -> action)

globalSharedMutableState :: MVar GlobalSharedMutableState
globalSharedMutableState = unsafePerformIO newEmptyMVar
{-# NOINLINE globalSharedMutableState #-}
