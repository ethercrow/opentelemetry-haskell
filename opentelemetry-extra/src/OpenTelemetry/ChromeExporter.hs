{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.ChromeExporter where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import OpenTelemetry.Common
import OpenTelemetry.Exporter
import System.IO

newtype ChromeBeginSpan = ChromeBegin Span

newtype ChromeEndSpan = ChromeEnd Span

newtype ChromeTagValue = ChromeTagValue TagValue

instance ToJSON ChromeTagValue where
  toJSON (ChromeTagValue (StringTagValue i)) = Data.Aeson.String i
  toJSON (ChromeTagValue (IntTagValue i)) = Data.Aeson.Number $ fromIntegral i
  toJSON (ChromeTagValue (BoolTagValue b)) = Data.Aeson.Bool b
  toJSON (ChromeTagValue (DoubleTagValue d)) = Data.Aeson.Number $ realToFrac d

instance ToJSON ChromeBeginSpan where
  toJSON (ChromeBegin Span {..}) =
    let threadId = case HM.lookup "tid" spanTags of
          Just (IntTagValue t) -> t
          _ -> 1
     in object
          [ "ph" .= ("B" :: String),
            "name" .= spanOperation,
            "pid" .= (1 :: Int),
            "tid" .= threadId,
            "ts" .= (div spanStartedAt 1000),
            "args" .= fmap ChromeTagValue spanTags
          ]

instance ToJSON ChromeEndSpan where
  toJSON (ChromeEnd Span {..}) =
    let threadId = case HM.lookup "tid" spanTags of
          Just (IntTagValue t) -> t
          _ -> 1
     in object
          [ "ph" .= ("E" :: String),
            "name" .= spanOperation,
            "pid" .= (1 :: Int),
            "tid" .= threadId,
            "ts" .= (div spanFinishedAt 1000)
          ]

createChromeSpanExporter :: FilePath -> IO (Exporter Span)
createChromeSpanExporter path = do
  f <- openFile path WriteMode
  hPutStrLn f "["
  pure
    $! Exporter
      ( \sps -> do
          mapM_
            ( \sp -> do
                LBS.hPutStr f $ encode $ ChromeBegin sp
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
