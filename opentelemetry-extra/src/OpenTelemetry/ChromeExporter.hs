{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.ChromeExporter where

import Data.HashMap.Strict as HM
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Word
import OpenTelemetry.EventlogStreaming_Internal
import OpenTelemetry.Common
import OpenTelemetry.Exporter
import System.IO
import Data.Function

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
      [ "ph" .= ("i" :: String)
      , "name" .= spanEventValue
      , "pid" .= (1 :: Int)
      , "tid" .= threadId
      , "ts" .= (div spanEventTimestamp 1000)
      ]

instance ToJSON ChromeBeginSpan where
  toJSON (ChromeBegin Span {..}) =
    object
      [ "ph" .= ("B" :: String),
        "name" .= spanOperation,
        "pid" .= (1 :: Int),
        "tid" .= spanThreadId,
        "ts" .= (div spanStartedAt 1000),
        "args" .= fmap ChromeTagValue
          (spanTags
            & HM.insert "gc_us" (IntTagValue . fromIntegral $ spanNanosecondsSpentInGC `div` 1000)
            & (if spanNanosecondsSpentInGC == 0
                then id
                else HM.insert "gc_fraction" (DoubleTagValue (fromIntegral spanNanosecondsSpentInGC / fromIntegral (spanFinishedAt - spanStartedAt)))))
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

createChromeSpanExporter :: FilePath -> IO (Exporter Span)
createChromeSpanExporter path = do
  f <- openFile path WriteMode
  hPutStrLn f "[ "
  pure
    $! Exporter
      ( \sps -> do
          mapM_
            ( \sp@Span{..} -> do
                LBS.hPutStr f $ encode $ ChromeBegin sp
                LBS.hPutStr f ",\n"
                forM_ (sortOn spanEventTimestamp spanEvents) $ \ev -> do
                  LBS.hPutStr f $ encode $ ChromeEvent spanThreadId ev
                  LBS.hPutStr f ",\n"
                LBS.hPutStr f $ encode $ ChromeEnd sp
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

eventlogToChrome :: FilePath -> FilePath -> IO ()
eventlogToChrome eventlogFile chromeFile = do
  exporter <- createChromeSpanExporter chromeFile
  exportEventlog exporter eventlogFile
