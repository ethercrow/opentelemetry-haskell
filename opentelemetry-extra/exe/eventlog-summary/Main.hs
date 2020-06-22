module Main where

import Control.Monad
import qualified Data.Text as T
import OpenTelemetry.Exporter
import OpenTelemetry.Common
import OpenTelemetry.EventlogStreaming_Internal
import System.Environment
import qualified Data.HashTable.IO as H
import Text.Printf
import Data.List (sortOn)
import Data.Word
import Data.IORef

type HashTable k v = H.BasicHashTable k v

data PerOperationStats = PerOperationStats
  { count :: !Int
  , min_ns :: !Word64
  , max_ns :: !Word64
  , total_ns :: !Word64
  }

data MetricStats = MetricStats
  { max_threads :: !Int
  , max_alloc_bytes :: !Int
  , max_live_bytes :: !Int
  }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      (opCounts:: H.CuckooHashTable T.Text PerOperationStats) <- H.new
      metricStats <- newIORef (MetricStats 0 0 0)
      -- max_threads <- newIORef 0
      -- max_alloc <- newIORef 0
      -- max_live <- newIORef 0
      let span_exporter = Exporter
            (\sps -> do
              forM_ sps $ \sp -> do
                let duration = spanFinishedAt sp - spanStartedAt sp
                H.mutate
                  opCounts
                  (spanOperation sp)
                  (\m -> (Just $ maybe
                                  (PerOperationStats 0 maxBound 0 0)
                                  (\PerOperationStats {..} ->
                                    PerOperationStats
                                    (count + 1)
                                    (min min_ns duration)
                                    (max max_ns duration)
                                    (total_ns + duration))
                                  m, ()))
              pure ExportSuccess)
            (pure ())
          metric_exporter = Exporter
              ( \metrics -> do
                  forM_ metrics $ \(Gauge _ label value) ->
                    modifyIORef metricStats $ \s -> case T.unpack label of
                      "threads" -> s { max_threads = max value (max_threads s) }
                      "heap_alloc_bytes" -> s { max_alloc_bytes = max value (max_alloc_bytes s) }
                      "heap_live_bytes" -> s { max_live_bytes = max value (max_live_bytes s) }
                      _ -> s
                  pure ExportSuccess
              )
              (pure ())
      exportEventlog span_exporter metric_exporter path
      leaderboard <- sortOn (total_ns . snd) <$> H.toList opCounts
      printf "Count\tTot ms\tMin ms\tMax ms\tOperation\n"
      printf "-----\t------\t------\t------\t---------\n"
      mapM_
        (\(op, PerOperationStats {..}) ->
          printf "%d\t%d\t%d\t%d\t%s\n"
            count
            (total_ns `div` 1000000)
            (min_ns `div` 1000000)
            (max_ns `div` 1000000)
            (T.unpack op))
        leaderboard

      putStrLn "---"

      MetricStats{ max_threads, max_alloc_bytes, max_live_bytes } <- readIORef metricStats
      printf "Max threads: %v\n" max_threads
      printf "Max allocated: %vMB\n" (max_alloc_bytes `div` 1000000)
      printf "Max live: %vMB\n" (max_live_bytes `div` 1000000)
      putStrLn "It's fine"
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-summary <program.eventlog>"
