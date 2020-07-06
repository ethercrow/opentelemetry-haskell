{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.Text as T
import OpenTelemetry.Common
import OpenTelemetry.Metrics
import OpenTelemetry.EventlogStreaming_Internal
import System.Environment
import Data.Char (isDigit)
import Data.Function
import qualified Data.HashTable.IO as H
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Word
import Graphics.Vega.VegaLite hiding (name)
import OpenTelemetry.Common
import OpenTelemetry.EventlogStreaming_Internal
import System.Environment
import Text.Printf

type HashTable k v = H.BasicHashTable k v

data PerOperationStats = PerOperationStats
  { count :: !Int,
    min_ns :: !Word64,
    max_ns :: !Word64,
    total_ns :: !Word64
  }

data MetricStats = MetricStats
  { max_threads :: !Int,
    -- | Per-capability
    total_alloc_bytes :: !(IntMap Int),
    max_live_bytes :: !Int
  }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      (opCounts :: H.CuckooHashTable T.Text PerOperationStats) <- H.new
      metricStats <- newIORef (MetricStats 0 IntMap.empty 0)
      let span_exporter =
            Exporter
              ( \sps -> do
                  forM_ sps $ \sp -> do
                    let duration = spanFinishedAt sp - spanStartedAt sp
                    H.mutate
                      opCounts
                      (spanOperation sp)
                      ( \m ->
                          ( Just $
                              maybe
                                (PerOperationStats 1 duration duration duration)
                                ( \PerOperationStats {..} ->
                                    PerOperationStats
                                      (count + 1)
                                      (min min_ns duration)
                                      (max max_ns duration)
                                      (total_ns + duration)
                                )
                                m,
                            ()
                          )
                      )
                  pure ExportSuccess
              )
              (pure ())
      metric_exporter <- aggregated $ Exporter
        ( \metrics -> do
            forM_ metrics $ \(AggregatedMetric (SomeInstrument instrument) (MetricDatapoint _ value)) ->
              modifyIORef metricStats $ \s -> case splitCapability $ T.unpack (instrumentName instrument) of
                (_, "threads") -> s { max_threads = max value (max_threads s) }
                (Just cap, "heap_alloc_bytes") -> s { total_alloc_bytes = IntMap.insert cap value (total_alloc_bytes s) }
                (_, "heap_live_bytes") -> s { max_live_bytes = max value (max_live_bytes s) }
                _ -> s
            pure ExportSuccess
        )
        (pure ())
      exportEventlog span_exporter metric_exporter path
      leaderboard <- sortOn (total_ns . snd) <$> H.toList opCounts
      printf "Count\tTot ms\tMin ms\tMax ms\tOperation\n"
      printf "-----\t------\t------\t------\t---------\n"
      mapM_
        ( \(op, PerOperationStats {..}) ->
            printf
              "%d\t%d\t%d\t%d\t%s\n"
              count
              (total_ns `div` 1000000)
              (min_ns `div` 1000000)
              (max_ns `div` 1000000)
              (T.unpack op)
        )
        leaderboard

      putStrLn "---"

      MetricStats {max_threads, total_alloc_bytes, max_live_bytes} <- readIORef metricStats
      printf "Max threads: %v\n" max_threads
      putStrLn "Total allocations:"
      _ <-
        IntMap.traverseWithKey
          (\cap bytes -> printf "  * Capability %v: %vMB\n" cap (bytes `div` 1000000))
          total_alloc_bytes
      printf "Max live: %vMB\n" (max_live_bytes `div` 1000000)

      putStrLn "---"

      let vega_visualization =
            toVegaLite
              [ title "Total duration of operation" [],
                vega_dat,
                mark Bar [],
                vega_enc
              ]
          vega_enc =
            []
              & position Y [PName "op", PmType Nominal, PAxis [AxTitle "operation"]]
              & position X [PName "dur", PmType Quantitative, PAxis [AxTitle "duration in nanoseconds"]]
              & encoding
          vega_dat =
            []
              & dataColumn "op" (Strings (map fst leaderboard))
              & dataColumn "dur" (Numbers (map (fromIntegral . total_ns . snd) leaderboard))
              & dataFromColumns []

      toHtmlFile "eventlog-summary.html" vega_visualization
      putStrLn "Generated report: eventlog-summary.html"
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-summary <program.eventlog>"

-- | Parse capability number out of event/metric name. If it fails, returns
-- (Nothing, the original string)
splitCapability :: String -> (Maybe Int, String)
splitCapability fullName@('c' : 'a' : 'p' : '_' : rest) =
  case span isDigit rest of
    (numStr, '_' : name) -> (Just (read numStr), name)
    _ -> (Nothing, fullName)
splitCapability name = (Nothing, name)
