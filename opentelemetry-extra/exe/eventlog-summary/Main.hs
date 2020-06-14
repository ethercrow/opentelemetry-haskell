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

type HashTable k v = H.BasicHashTable k v

data PerOperationStats = Stats
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
      let exporter = Exporter
            (\sps -> do
              forM_ sps $ \sp -> do
                let duration = spanFinishedAt sp - spanStartedAt sp
                H.mutate
                  opCounts
                  (spanOperation sp)
                  (\m -> (Just $ maybe
                                  (Stats 0 maxBound 0 0)
                                  (\Stats {..} ->
                                    Stats
                                    (count + 1)
                                    (min min_ns duration)
                                    (max max_ns duration)
                                    (total_ns + duration))
                                  m, ()))
              pure ExportSuccess)
            (pure ())
      exportEventlog exporter path
      leaderboard <- sortOn (total_ns . snd) <$> H.toList opCounts
      printf "Count\tTot ms\tMin ms\tMax ms\tOperation\n"
      printf "-----\t------\t------\t------\t---------\n"
      mapM_
        (\(op, Stats {..}) ->
          printf "%d\t%d\t%d\t%d\t%s\n"
            count
            (total_ns `div` 1000000)
            (min_ns `div` 1000000)
            (max_ns `div` 1000000)
            (T.unpack op))
        leaderboard
      putStrLn "It's fine"
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-summary <program.eventlog>"
