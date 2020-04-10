module OpenTelemetry.Tracer where

import OpenTelemetry.Common
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty as NE
import Data.Hashable

data Tracer threadId
  = Tracer
      { tracerSpanStacks :: !(HM.HashMap threadId (NE.NonEmpty Span))
      }
      deriving (Eq, Show)

tracerPushSpan :: (Eq tid, Hashable tid) => Tracer tid -> tid -> Span -> Tracer tid
tracerPushSpan t@(Tracer {..}) tid sp =
  case HM.lookup tid tracerSpanStacks of
    Nothing ->
      let !stacks = HM.insert tid (sp :| []) tracerSpanStacks
      in Tracer stacks
    Just sps ->
      let !stacks = HM.insert tid (sp <| sps) tracerSpanStacks
      in t { tracerSpanStacks = stacks }

tracerPopSpan :: (Eq tid, Hashable tid) => Tracer tid -> tid -> (Maybe Span, Tracer tid)
tracerPopSpan t@(Tracer {..}) tid =
  case HM.lookup tid tracerSpanStacks of
    Nothing -> (Nothing, t)
    Just (sp :| sps) ->
      let stacks =
            case NE.nonEmpty sps of
              Nothing -> HM.delete tid tracerSpanStacks
              Just sps' -> HM.insert tid sps' tracerSpanStacks
       in (Just sp, Tracer stacks)

tracerGetCurrentActiveSpan :: (Hashable tid, Eq tid) => Tracer tid -> tid -> Maybe Span
tracerGetCurrentActiveSpan (Tracer stacks) tid =
  case HM.lookup tid stacks of
    Nothing -> Nothing
    Just (sp NE.:| _) -> Just sp

createTracer :: (Hashable tid, Eq tid) => IO (Tracer tid)
createTracer = pure $ Tracer mempty

