{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.ChromeExporter where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import Data.Function
import Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Word
import OpenTelemetry.Common
import System.IO
import OpenTelemetry.Metrics (instrumentName, SomeInstrument(SomeInstrument))
import OpenTelemetry.EventlogStreaming_Internal

newtype ChromeBeginSpan = ChromeBegin Span

newtype ChromeEndSpan = ChromeEnd Span

newtype ChromeTagValue = ChromeTagValue TagValue

data ChromeEvent = ChromeEvent Word32 SpanEvent

instance ToJSON ChromeTagValue where
  toJSON (ChromeTagValue (StringTagValue (TagVal i))) = Data.Aeson.String i
  toJSON (ChromeTagValue (IntTagValue i)) = Data.Aeson.Number $ fromIntegral i
  toJSON (ChromeTagValue (BoolTagValue b)) = Data.Aeson.Bool b
  toJSON (ChromeTagValue (DoubleTagValue d)) = Data.Aeson.Number $ realToFrac d

instance ToJSON ChromeEvent where
  toJSON (ChromeEvent threadId SpanEvent {..}) =
    object
      [ "ph" .= ("i" :: String),
        "name" .= spanEventValue,
        "pid" .= (1 :: Int),
        "tid" .= threadId,
        "ts" .= (div spanEventTimestamp 1000)
      ]

instance ToJSON ChromeBeginSpan where
  toJSON (ChromeBegin Span {..}) =
    object
      [ "ph" .= ("B" :: String),
        "name" .= spanOperation,
        "pid" .= (1 :: Int),
        "tid" .= spanThreadId,
        "ts" .= (div spanStartedAt 1000),
        "args"
          .= fmap
            ChromeTagValue
            ( spanTags
                & HM.insert "gc_us" (IntTagValue . fromIntegral $ spanNanosecondsSpentInGC `div` 1000)
                & ( if spanNanosecondsSpentInGC == 0
                      then id
                      else HM.insert "gc_fraction" (DoubleTagValue (fromIntegral spanNanosecondsSpentInGC / fromIntegral (spanFinishedAt - spanStartedAt)))
                  )
            )
      ]

instance ToJSON ChromeEndSpan where
  toJSON (ChromeEnd Span {..}) =
    object
      [ "ph" .= ("E" :: String),
        "name" .= spanOperation,
        "pid" .= (1 :: Int),
        "tid" .= spanThreadId,
        "ts" .= (div spanFinishedAt 1000)
      ]

createChromeExporter :: FilePath -> IO (Exporter Span, Exporter Metric)
createChromeExporter path = createChromeExporter' path SplitThreads

createChromeExporter' :: FilePath -> DoWeCollapseThreads -> IO (Exporter Span, Exporter Metric)
createChromeExporter' path doWeCollapseThreads = do
  f <- openFile path WriteMode
  hPutStrLn f "[ "
  let modifyThreadId = case doWeCollapseThreads of
        CollapseThreads -> const 1
        SplitThreads -> id
      span_exporter =
        Exporter
          ( \sps -> do
              mapM_
                ( \sp -> do
                    let sp' = sp {spanThreadId = modifyThreadId (spanThreadId sp)}
                    let Span {spanThreadId, spanEvents} = sp'
                    LBS.hPutStr f $ encode $ ChromeBegin sp'
                    LBS.hPutStr f ",\n"
                    forM_ (sortOn spanEventTimestamp spanEvents) $ \ev -> do
                      LBS.hPutStr f $ encode $ ChromeEvent (modifyThreadId spanThreadId) ev
                      LBS.hPutStr f ",\n"
                    LBS.hPutStr f $ encode $ ChromeEnd sp'
                    LBS.hPutStr f ",\n"
                )
                sps
              pure ExportSuccess
          )
          ( do
              hSeek f RelativeSeek (-2) -- overwrite the last comma
              hPutStrLn f "\n]"
              hClose f
          )
  metric_exporter <- aggregated $ Exporter
    ( \metrics -> do
        forM_ metrics $ \(AggregatedMetric (SomeInstrument (TE.decodeUtf8 . instrumentName -> name)) (MetricDatapoint ts value)) -> do
          LBS.hPutStr f $ encode $
            object
                  [ "ph" .= ("C" :: String),
                    "name" .= name,
                    "ts" .= (div ts 1000),
                    "args" .= object [name .= Number (fromIntegral value)]
                  ]
          LBS.hPutStr f ",\n"
        pure ExportSuccess
    )
    (pure ())
  pure (span_exporter, metric_exporter)

data DoWeCollapseThreads = CollapseThreads | SplitThreads

eventlogToChrome :: FilePath -> FilePath -> DoWeCollapseThreads -> IO ()
eventlogToChrome eventlogFile chromeFile doWeCollapseThreads = do
  (span_exporter, metric_exporter) <- createChromeExporter' chromeFile doWeCollapseThreads
  exportEventlog span_exporter metric_exporter eventlogFile
  shutdown span_exporter
  shutdown metric_exporter
