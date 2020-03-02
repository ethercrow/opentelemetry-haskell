{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Implicit where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe
import qualified Data.Text as T
import OpenTelemetry.Common
import OpenTelemetry.FileExporter
import System.IO.Unsafe
import System.Random

data SpanChildness = Root | ChildOf Span

withSpan :: (MonadIO m, MonadMask m) => String -> m a -> m a
withSpan = generalWithSpan Root

-- TODO(divanov): add ability to override automatic error=true tagging on exceptions
generalWithSpan :: (MonadIO m, MonadMask m) => SpanChildness -> String -> m a -> m a
generalWithSpan childness operation action = do
  threadId <- liftIO myThreadId
  sid <- liftIO randomIO
  startedAt <- liftIO now64
  (result, ()) <-
    generalBracket
      ( liftIO $ modifyMVar_ globalSharedMutableState $ \GlobalSharedMutableState {..} -> do
          let (!mpsid, !ctx) = case (childness, HM.lookup threadId (tracerSpanStacks gTracer)) of
                (ChildOf parent, _) -> (Just (spanId parent), SpanContext (SId sid) (spanTraceId parent))
                (_, Nothing) -> (Nothing, SpanContext (SId sid) (TId sid))
                (_, Just ((spanContext -> SpanContext psid tid) :| _)) -> (Just psid, SpanContext (SId sid) tid)
              !sp = Span ctx (T.pack operation) startedAt 0 (HM.singleton "thread_id" (StringTagValue $ T.pack $ show threadId)) mempty OK mpsid
              !tracer = tracerPushSpan gTracer threadId sp
          pure $! GlobalSharedMutableState gSpanExporter tracer
      )
      ( \_ exitcase -> do
          liftIO $ modifyMVar_ globalSharedMutableState $ \GlobalSharedMutableState {..} -> do
            let (mspan, tracer) = tracerPopSpan gTracer threadId
            case mspan of
              Nothing -> pure ()
              Just sp -> do
                finishedAt <- liftIO now64
                let sp' = case exitcase of
                      ExitCaseSuccess _ -> sp
                      _ -> sp {spanTags = HM.insert "error" (BoolTagValue True) (spanTags sp)}
                res <- export gSpanExporter [sp' {spanFinishedAt = finishedAt}]
                case res of
                  ExportSuccess -> pure ()
                  _ -> error $ "exporting span failed: " <> show sp
            pure $! GlobalSharedMutableState gSpanExporter tracer
      )
      (\_ -> action)
  pure result

setTag :: forall value m. (MonadIO m, ToTagValue value) => T.Text -> value -> m ()
setTag k v =
  modifyCurrentSpan
    ( \sp ->
        sp {spanTags = HM.insert k (toTagValue v) (spanTags sp)}
    )

addEvent :: forall m. MonadIO m => T.Text -> T.Text -> m ()
addEvent name value = do
  now <- liftIO now64
  modifyCurrentSpan
    ( \sp ->
        sp {spanEvents = SpanEvent now name value : spanEvents sp}
    )

setParentSpanContext :: MonadIO m => SpanContext -> m ()
setParentSpanContext (SpanContext psid tid) =
  modifyCurrentSpan
    ( \sp ->
        sp
          { spanContext = SpanContext (spanId sp) tid,
            spanParentId = Just psid
          }
    )

withOpenTelemetry :: (MonadIO m, MonadMask m) => OpenTelemetryConfig -> m a -> m a
withOpenTelemetry OpenTelemetryConfig {..} action = do
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

withZeroConfigOpenTelemetry :: (MonadIO m, MonadMask m) => m a -> m a
withZeroConfigOpenTelemetry action = do
  -- TODO(divanov): crossplatformer temporary directory
  -- TODO(divanov): include program name and current date in the filename
  exporter <- liftIO $ createFileSpanExporter "/tmp/opentelemetry.trace.json"
  let otelConfig = OpenTelemetryConfig {otcSpanExporter = exporter}
  withOpenTelemetry otelConfig action

getCurrentSpanContext :: MonadIO m => m (Maybe SpanContext)
getCurrentSpanContext = do
  tid <- liftIO myThreadId
  GlobalSharedMutableState {..} <- liftIO $ readMVar globalSharedMutableState
  pure $ spanContext <$> tracerGetCurrentActiveSpan gTracer tid

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
             in pure $! g {gTracer = gTracer {tracerSpanStacks = stacks}}
    )

-- TODO(divanov): try to merge withSpan and withChildSpanOf
withChildSpanOf :: (MonadIO m, MonadMask m) => Span -> String -> m a -> m a
withChildSpanOf parent operation action = generalWithSpan (ChildOf parent) operation action

globalSharedMutableState :: MVar GlobalSharedMutableState
globalSharedMutableState = unsafePerformIO newEmptyMVar
{-# NOINLINE globalSharedMutableState #-}
