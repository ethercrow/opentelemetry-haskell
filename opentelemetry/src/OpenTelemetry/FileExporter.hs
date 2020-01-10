module OpenTelemetry.FileExporter where

import OpenTelemetry.Common
import System.IO

createFileSpanExporter :: FilePath -> IO (Exporter Span)
createFileSpanExporter path = do
  f <- openFile path WriteMode
  pure
    $! Exporter
      ( \sps -> do
          mapM_ (hPrint f) sps
          pure ExportSuccess
      )
      (hClose f)
