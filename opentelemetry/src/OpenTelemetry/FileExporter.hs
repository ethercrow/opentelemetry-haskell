module OpenTelemetry.FileExporter where

import OpenTelemetry.Common
import System.IO
import Text.Printf

showSpan :: Span -> String
showSpan s@(Span {..}) =
  let (TId tid) = spanTraceId s
   in printf
        "{\"ph\":\"B\",\"name\":\"%s\",\"ts\":%d,\"tid\":%d},{\"ph\":\"E\",\"name\":\"%s\",\"ts\":%d,\"tid\":%d},"
        spanOperation
        (div spanStartedAt 1000)
        tid
        spanOperation
        (div spanFinishedAt 1000)
        tid

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
