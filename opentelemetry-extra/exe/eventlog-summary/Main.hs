module Main where

import Control.Monad
import qualified Data.Text as T
import OpenTelemetry.Exporter
import OpenTelemetry.Common
import OpenTelemetry.EventlogStreaming_Internal
import System.Environment
import qualified Data.HashTable.IO as H
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Text.Printf
import Data.List (sortOn)
import Data.Word
import Data.IORef
import Data.Char (isDigit)

type HashTable k v = H.BasicHashTable k v

data PerOperationStats = PerOperationStats
  { count :: !Int
  , min_ns :: !Word64
  , max_ns :: !Word64
  , total_ns :: !Word64
  }

data MetricStats = MetricStats
  { max_threads :: !Int
  , total_alloc_bytes :: !(IntMap Int)
    -- ^ Per-capability
  , max_live_bytes :: !Int
  }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      (opCounts:: H.CuckooHashTable T.Text PerOperationStats) <- H.new
      metricStats <- newIORef (MetricStats 0 IntMap.empty 0)
      let span_exporter = Exporter
            (\sps -> do
              forM_ sps $ \sp -> do
                let duration = spanFinishedAt sp - spanStartedAt sp
                H.mutate
                  opCounts
                  (spanOperation sp)
                  (\m -> (Just $ maybe
                                  (PerOperationStats 1 duration duration duration)
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
                    modifyIORef metricStats $ \s -> case splitCapability $ T.unpack label of
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
        (\(op, PerOperationStats {..}) ->
          printf "%d\t%d\t%d\t%d\t%s\n"
            count
            (total_ns `div` 1000000)
            (min_ns `div` 1000000)
            (max_ns `div` 1000000)
            (T.unpack op))
        leaderboard

      putStrLn "---"

      MetricStats{ max_threads, total_alloc_bytes, max_live_bytes } <- readIORef metricStats
      printf "Max threads: %v\n" max_threads
      putStrLn "Total allocations:"
      _ <- IntMap.traverseWithKey
        (\cap bytes -> printf "  * Capability %v: %vMB\n" cap (bytes `div` 1000000))
        total_alloc_bytes
      printf "Max live: %vMB\n" (max_live_bytes `div` 1000000)
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-summary <program.eventlog>"

-- | Parse capability number out of event/metric name. If it fails, returns
-- (Nothing, the original string)
splitCapability :: String -> (Maybe Int, String)
splitCapability fullName@('c':'a':'p':'_':rest) =
  case span isDigit rest of
    (numStr, '_':name) -> (Just (read numStr), name)
    _ -> (Nothing, fullName)
splitCapability name = (Nothing, name)
