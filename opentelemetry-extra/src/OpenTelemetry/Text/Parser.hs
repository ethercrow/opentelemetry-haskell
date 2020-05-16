{-# LANGUAGE OverloadedStrings #-}
module OpenTelemetry.Text.Parser where

import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text as T
import Data.Word
import GHC.Stack
import OpenTelemetry.Binary.Eventlog
import OpenTelemetry.Common
import OpenTelemetry.Handler
import qualified OpenTelemetry.Parser as P
import OpenTelemetry.SpanContext
import Text.Printf


parseText :: [T.Text]
          -> P.State
          -> (Word32, Timestamp, Maybe TraceId)
          -> (LogEvent -> P.State -> (Word32, Timestamp, Maybe TraceId) -> (P.State,[Span]))
          -> (P.State, [Span])
parseText msgWords st config handler =
    case msgWords of
      ("ot2" : "begin" : "span" : serial_text : name) ->
        let serial = read (T.unpack serial_text)
            operation = T.intercalate " " name
         in handler (BeginSpanEv (SpanInFlight serial) operation) st config
      ["ot2", "end", "span", serial_text] ->
        let serial = read (T.unpack serial_text)
         in handler (EndSpanEv (SpanInFlight serial)) st config
      ("ot2" : "set" : "tag" : serial_text : k : v) ->
        let serial = read (T.unpack serial_text)
         in handler (TagEv (SpanInFlight serial) k $ T.unwords v) st config
      ["ot2", "set", "traceid", serial_text, trace_id_text] ->
        let serial = read (T.unpack serial_text)
            trace_id = TId (read ("0x" <> T.unpack trace_id_text))
         in handler (SetTraceEv (SpanInFlight serial) trace_id) st config
      ["ot2", "set", "spanid", serial_text, new_span_id_text] ->
        let serial = read (T.unpack serial_text)
            span_id = (SId (read ("0x" <> T.unpack new_span_id_text)))
         in handler (SetSpanEv (SpanInFlight serial) span_id) st config
      ["ot2", "set", "parent", serial_text, trace_id_text, parent_span_id_text] ->
        let trace_id = TId (read ("0x" <> T.unpack trace_id_text))
            serial = read (T.unpack serial_text)
            psid = SId (read ("0x" <> T.unpack parent_span_id_text))
         in handler (SetParentEv (SpanInFlight serial)
                                     (SpanContext psid trace_id))
                  st config
      ("ot2" : "add" : "event" : serial_text : k : v) ->
        let serial = read (T.unpack serial_text)
         in handler (EventEv (SpanInFlight serial) k $ T.unwords v) st config
      ("ot2" : rest) -> error $ printf "Unrecognized %s" (show rest)
      _ -> (st, [])
