module OpenTelemetry.FileExporter where

import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
import OpenTelemetry.Common
import System.IO
import Text.Printf

showValue :: TagValue -> String
showValue (StringTagValue s) = show s
showValue (IntTagValue i) = show i

showSpan :: Span -> String
showSpan s@(Span {..}) =
  let (TId tid) = spanTraceId s
      meta :: String
      meta =
        spanTags
          & HM.toList
          & map (\(k, v) -> ["\"", T.unpack k, "\":", showValue v])
          & intersperse [","]
          & concat
          & concat
   in printf
        "{\"ph\":\"B\",\"name\":\"%s\",\"pid\":1,\"ts\":%d,\"tid\":%d,\"meta\":{%s}},{\"ph\":\"E\",\"name\":\"%s\",\"pid\":1,\"ts\":%d,\"tid\":%d},"
        spanOperation
        (div spanStartedAt 1000)
        tid
        meta
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