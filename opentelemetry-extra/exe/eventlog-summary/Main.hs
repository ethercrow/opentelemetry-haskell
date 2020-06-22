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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      (opCounts:: H.CuckooHashTable T.Text PerOperationStats) <- H.new
      max_threads <- newIORef 0
      max_alloc <- newIORef 0
      max_live <- newIORef 0
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
                  forM_ metrics $ \(Gauge time label value) ->
                    case T.unpack label of
                      "threads" -> modifyIORef max_threads (max value)
                      "heap_alloc_bytes" -> modifyIORef max_alloc (max value)
                      "heap_live_bytes" -> modifyIORef max_live (max value)
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
      printf "Max threads: %v\n"  =<< readIORef max_threads
      printf "Max allocated: %vMB\n" . (`div` 10^6) =<< readIORef max_alloc
      printf "Max live: %vMB\n" . (`div` 10^6) =<< readIORef max_live
      putStrLn "It's fine"
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-summary <program.eventlog>"
