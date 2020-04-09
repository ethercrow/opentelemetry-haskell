{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Implicit where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import OpenTelemetry.Common
import OpenTelemetry.Exporter
import OpenTelemetry.FileExporter
import OpenTelemetry.SpanContext
import OpenTelemetry.Tracer
import System.Directory
import System.Environment
import System.IO.Unsafe
import System.Random
import Text.Printf

data SpanChildness = Root | ChildOf Span

data AutoTagError = DoAutoTagError | Don'tAutoTagError

withSpan :: (MonadIO m, MonadMask m) => String -> m a -> m a
withSpan = generalWithSpan (WithSpanOptions Root DoAutoTagError)

data WithSpanOptions
  = WithSpanOptions
      { childness :: SpanChildness,
        autoTagError :: AutoTagError
      }

generalWithSpan :: (MonadIO m, MonadMask m) => WithSpanOptions -> String -> m a -> m a
generalWithSpan WithSpanOptions {childness, autoTagError} operation action = do
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
                let sp' = case (exitcase, autoTagError) of
                      (ExitCaseSuccess {}, _) -> sp
                      (_, Don'tAutoTagError) -> sp
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
        modifyMVar_ globalSharedMutableState (\_ -> pure $ GlobalSharedMutableState otcSpanExporter tracer)
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
  (now, prog_name, tmp_dir) <- liftIO $ (,,) <$> now64 <*> getProgName <*> getTemporaryDirectory
  exporter <- liftIO $ createFileSpanExporter $ printf "%s/%s-%d.trace.json" tmp_dir prog_name now
  let otelConfig = OpenTelemetryConfig {otcSpanExporter = exporter}
  withOpenTelemetry otelConfig action

getCurrentSpanContext :: MonadIO m => m (Maybe SpanContext)
getCurrentSpanContext = do
  tid <- liftIO myThreadId
  GlobalSharedMutableState {..} <- liftIO $ readMVar globalSharedMutableState
  pure $ spanContext <$> tracerGetCurrentActiveSpan gTracer tid

getCurrentActiveSpan :: MonadIO m => m (Maybe Span)
getCurrentActiveSpan = do
  tid <- liftIO myThreadId
  GlobalSharedMutableState {..} <- liftIO $ readMVar globalSharedMutableState
  pure $ tracerGetCurrentActiveSpan gTracer tid

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

withChildSpanOf :: (MonadIO m, MonadMask m) => Span -> String -> m a -> m a
withChildSpanOf parent operation action = generalWithSpan (WithSpanOptions (ChildOf parent) DoAutoTagError) operation action

globalSharedMutableState :: MVar GlobalSharedMutableState
globalSharedMutableState = unsafePerformIO $ do
  tracer <- createTracer
  newMVar (GlobalSharedMutableState noopExporter tracer)
{-# NOINLINE globalSharedMutableState #-}
