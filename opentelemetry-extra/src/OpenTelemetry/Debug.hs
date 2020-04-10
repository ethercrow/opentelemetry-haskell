module OpenTelemetry.Debug where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import System.Environment
import System.IO
import System.IO.Unsafe

dd_ :: Show a => String -> a -> IO ()
dd_ = unsafePerformIO $
  lookupEnv "OPENTELEMETRY_DEBUG" >>= \case
    Nothing -> pure $ \_ _ -> pure ()
    Just "0" -> pure $ \_ _ -> pure ()
    Just "false" -> pure $ \_ _ -> pure ()
    _ -> pure $ \s thing -> liftIO $ do
      hPutStr stderr (s <> ": ")
      hPutStrLn stderr (show thing)
{-# NOINLINE dd_ #-}

inc :: Int -> TVar Int -> IO ()
inc amount counterVar = atomically $ modifyTVar counterVar (+ amount)
