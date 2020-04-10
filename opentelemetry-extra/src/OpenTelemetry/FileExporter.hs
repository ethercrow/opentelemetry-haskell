{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.FileExporter where

import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.List (intersperse)
import qualified Data.Text as T
import OpenTelemetry.Common
import OpenTelemetry.Exporter
import OpenTelemetry.SpanContext
import System.IO
import Text.Printf
import Text.Read

showValue :: TagValue -> String
showValue (StringTagValue s) = show s
showValue (IntTagValue i) = show i
showValue _ = "\"unknown\""

showSpan :: Span -> String
showSpan s@(Span {..}) =
  let (TId tid) = spanTraceId s
      threadId = case HM.lookup "thread_id" spanTags of
        Just (StringTagValue (T.stripPrefix "ThreadId " -> Just (readMaybe . T.unpack -> Just t))) -> t
        Just (IntTagValue t) -> t
        _ -> 1
      meta :: String
      meta =
        spanTags
          & HM.toList
          & map (\(k, v) -> ["\"", T.unpack k, "\":", showValue v])
          & ([printf "\"traceId\":\"%x\"" tid] :)
          & intersperse [","]
          & concat
          & concat
   in printf
        "{\"ph\":\"B\",\"name\":\"%s\",\"pid\":1,\"ts\":%d,\"tid\":%d,\"args\":{%s}},{\"ph\":\"E\",\"name\":\"%s\",\"pid\":1,\"ts\":%d,\"tid\":%d},"
        spanOperation
        (div spanStartedAt 1000)
        threadId
        meta
        spanOperation
        (div spanFinishedAt 1000)
        threadId

createFileSpanExporter :: FilePath -> IO (Exporter Span)
createFileSpanExporter path = do
  f <- openFile path WriteMode
  hPutStrLn f "["
  pure
    $! Exporter
      ( \sps -> do
          mapM_ (hPutStrLn f . showSpan) sps
          pure ExportSuccess
      )
      ( do
          hSeek f RelativeSeek (-2) -- overwrite the last comma
          hPutStrLn f "\n]"
          hClose f
      )
