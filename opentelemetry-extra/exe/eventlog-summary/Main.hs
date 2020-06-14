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

type HashTable k v = H.BasicHashTable k v

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      (opCounts:: H.CuckooHashTable T.Text Int) <- H.new
      let exporter = Exporter
            (\sps -> do
              forM_ sps $ \sp -> do
                H.mutate opCounts (spanOperation sp) (\m -> (Just $ maybe 0 (+ 1) m, ()))
              pure ExportSuccess)
            (pure ())
      exportEventlog exporter path
      leaderboard <- sortOn snd <$> H.toList opCounts
      printf "Count\tOperation\n"
      printf "-----\t---------\n"
      mapM_
        (\(op, count) -> printf "%d\t%s\n" count (T.unpack op))
        leaderboard
      putStrLn "It's fine"
    _ -> do
      putStrLn "Usage:"
      putStrLn ""
      putStrLn "  eventlog-summary <program.eventlog>"
