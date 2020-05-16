{-# LANGUAGE GADTs #-}
module OpenTelemetry.Handler where

import qualified Data.ByteString.Lazy as LBS
import OpenTelemetry.Binary.Eventlog (SpanInFlight (..))
import OpenTelemetry.SpanContext

data MsgHandler where
    BeginSpanHandler :: SpanInFlight
                     -> LBS.ByteString
                     -> MsgHandler
    EndSpanHandler   :: SpanInFlight
                     -> MsgHandler
    TagHandler       :: SpanInFlight
                     -> LBS.ByteString
                     -> LBS.ByteString
                     -> MsgHandler
    EventHandler     :: SpanInFlight
                     -> LBS.ByteString
                     -> LBS.ByteString
                     -> MsgHandler
    SetParentHandler :: SpanInFlight
                     -> SpanContext
                     -> MsgHandler
    SetTraceHandler  :: SpanInFlight
                     -> TraceId
                     -> MsgHandler
    SetSpanHandler   :: SpanInFlight
                     -> SpanId
                     -> MsgHandler
