{-# LANGUAGE OverloadedStrings #-}
module OpenTelemetry.Text.Parser where

import Data.Text as T
import OpenTelemetry.Binary.Eventlog
import OpenTelemetry.Handler
import OpenTelemetry.SpanContext
import Text.Printf


parseText :: [T.Text] -> Maybe LogEvent
parseText =
    \case
      ("ot2" : "begin" : "span" : serial_text : name) ->
        let serial = read (T.unpack serial_text)
            operation = T.intercalate " " name
         in Just $ BeginSpanEv (SpanInFlight serial) operation
      ["ot2", "end", "span", serial_text] ->
        let serial = read (T.unpack serial_text)
         in Just $ EndSpanEv (SpanInFlight serial)
      ("ot2" : "set" : "tag" : serial_text : k : v) ->
        let serial = read (T.unpack serial_text)
         in Just $ TagEv (SpanInFlight serial) k $ T.unwords v
      ["ot2", "set", "traceid", serial_text, trace_id_text] ->
        let serial = read (T.unpack serial_text)
            trace_id = TId (read ("0x" <> T.unpack trace_id_text))
         in Just $ SetTraceEv (SpanInFlight serial) trace_id
      ["ot2", "set", "spanid", serial_text, new_span_id_text] ->
        let serial = read (T.unpack serial_text)
            span_id = (SId (read ("0x" <> T.unpack new_span_id_text)))
         in Just $ SetSpanEv (SpanInFlight serial) span_id
      ["ot2", "set", "parent", serial_text, trace_id_text, parent_span_id_text] ->
        let trace_id = TId (read ("0x" <> T.unpack trace_id_text))
            serial = read (T.unpack serial_text)
            psid = SId (read ("0x" <> T.unpack parent_span_id_text))
         in Just $ SetParentEv (SpanInFlight serial)
                (SpanContext psid trace_id)
      ("ot2" : "add" : "event" : serial_text : k : v) ->
        let serial = read (T.unpack serial_text)
         in Just . EventEv (SpanInFlight serial) k $ T.unwords v
      ("ot2" : rest) -> error $ printf "Unrecognized %s" (show rest)
      _ -> Nothing
