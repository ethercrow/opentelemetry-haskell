{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.ChromeExporter where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Coerce
import Data.Function
import Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.Text.Encoding as TE
import Data.Word
import qualified Jsonifier as J
import OpenTelemetry.Common
import OpenTelemetry.EventlogStreaming_Internal
import System.IO

newtype ChromeBeginSpan = ChromeBegin Span

newtype ChromeEndSpan = ChromeEnd Span

data ChromeEvent = ChromeEvent Word32 SpanEvent

jTagValue :: TagValue -> J.Json
jTagValue (StringTagValue (TagVal i)) = J.textString i
jTagValue (IntTagValue i) = J.intNumber i
jTagValue (BoolTagValue b) = J.bool b
jTagValue (DoubleTagValue d) = J.doubleNumber d

jChromeEvent (ChromeEvent threadId SpanEvent {..}) =
  J.object
    [ ("ph", J.textString "i"),
      ("name", J.textString $ coerce spanEventValue),
      ("pid", J.intNumber 1),
      ("tid", J.wordNumber $ fromIntegral threadId),
      ("ts", J.intNumber . fromIntegral $ div spanEventTimestamp 1000)
    ]

jChromeBeginSpan Span {..} =
  J.object
    [ ("ph", J.textString "B"),
      ("name", J.textString spanOperation),
      ("pid", J.intNumber 1),
      ("tid", J.intNumber $ fromIntegral spanDisplayThreadId),
      ("ts", J.wordNumber . fromIntegral $ div spanStartedAt 1000),
      ( "args",
        J.object
          ( spanTags
              & HM.insert "gc_us" (IntTagValue . fromIntegral $ spanNanosecondsSpentInGC `div` 1000)
              & ( if spanNanosecondsSpentInGC == 0
                    then id
                    else HM.insert "gc_fraction" (DoubleTagValue (fromIntegral spanNanosecondsSpentInGC / fromIntegral (spanFinishedAt - spanStartedAt)))
                )
              & HM.toList
              & fmap (\(TagName n, v) -> (n, jTagValue v))
          )
      )
    ]

jChromeEndSpan Span {..} =
  J.object
    [ ("ph", J.textString "E"),
      ("name", J.textString spanOperation),
      ("pid", J.intNumber 1),
      ("tid", J.intNumber $ fromIntegral spanDisplayThreadId),
      ("ts", J.wordNumber . fromIntegral $ div spanFinishedAt 1000)
    ]

createChromeExporter :: FilePath -> IO (Exporter Span, Exporter Metric)
createChromeExporter path = createChromeExporter' path SplitThreads

createChromeExporter' :: FilePath -> ThreadPresentation -> IO (Exporter Span, Exporter Metric)
createChromeExporter' path threadPresentation = do
  f <- openFile path WriteMode
  hPutStrLn f "[ "
  let modifyThreadId = case threadPresentation of
        CollapseThreads -> pure . const 1
        SplitThreads -> pure
      span_exporter =
        Exporter
          ( \sps -> do
              mapM_
                ( \sp@(Span {spanEvents}) -> do
                    tid' <- modifyThreadId (spanDisplayThreadId sp)
                    let sp' = sp {spanDisplayThreadId = tid'}
                    BS.hPutStr f $ J.toByteString $ jChromeBeginSpan sp'
                    BS.hPutStr f ",\n"
                    forM_ (sortOn spanEventTimestamp spanEvents) $ \ev -> do
                      BS.hPutStr f $ J.toByteString $ jChromeEvent $ ChromeEvent tid' ev
                      BS.hPutStr f ",\n"
                    BS.hPutStr f $ J.toByteString $ jChromeEndSpan sp'
                    BS.hPutStr f ",\n"
                )
                sps
              pure ExportSuccess
          )
          ( do
              hSeek f RelativeSeek (-2) -- overwrite the last comma
              hPutStrLn f "\n]"
              hClose f
          )
  metric_exporter <-
    aggregated $
      Exporter
        ( \metrics -> do
            -- forM_ metrics $ \(AggregatedMetric (SomeInstrument (TE.decodeUtf8 . instrumentName -> name)) (MetricDatapoint ts value)) -> do
            forM_ metrics $ \(AggregatedMetric (CaptureInstrument _ (TE.decodeUtf8 -> name)) (MetricDatapoint ts value)) -> do
              BS.hPutStr f $
                J.toByteString $
                  J.object
                    [ ("ph", J.textString "C"),
                      ("name", J.textString name),
                      ("ts", J.wordNumber $ fromIntegral $ div ts 1000),
                      ("args", J.object [(name, J.intNumber value)])
                    ]
              BS.hPutStr f ",\n"
            pure ExportSuccess
        )
        (pure ())
  pure (span_exporter, metric_exporter)

data ThreadPresentation = CollapseThreads | SplitThreads

eventlogToChrome :: FilePath -> FilePath -> ThreadPresentation -> IO ()
eventlogToChrome eventlogFile chromeFile doWeCollapseThreads = do
  (span_exporter, metric_exporter) <- createChromeExporter' chromeFile doWeCollapseThreads
  exportEventlog span_exporter metric_exporter eventlogFile
  shutdown span_exporter
  shutdown metric_exporter
