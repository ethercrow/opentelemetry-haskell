{-# LANGUAGE OverloadedStrings #-}

module LightStep.Config where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP2.Client
import System.Environment
import System.IO

data LightStepConfig
  = LightStepConfig
      { lsHostName :: HostName,
        lsPort :: PortNumber,
        lsToken :: T.Text,
        lsServiceName :: T.Text,
        lsGlobalTags :: [(T.Text, T.Text)],
        lsGracefulShutdownTimeoutSeconds :: Int
      }

lookupOneOfEnvs :: [String] -> IO (Maybe String)
lookupOneOfEnvs names = asum <$> traverse lookupEnv names

getEnvTagsWithPrefix :: T.Text -> IO [(T.Text, T.Text)]
getEnvTagsWithPrefix prefix =
  mapMaybe unprefix <$> getEnvironment
  where
    unprefix ((T.stripPrefix prefix . T.pack) -> Just k, v) = Just (k, T.pack v)
    unprefix _ = Nothing

getEnvConfig :: MonadIO m => m (Maybe LightStepConfig)
getEnvConfig = liftIO $ do
  maybe_token_from_env <- lookupOneOfEnvs ["LIGHTSTEP_TOKEN", "LIGHTSTEP_ACCESS_TOKEN", "OPENTRACING_LIGHTSTEP_ACCESS_TOKEN"]
  global_tags <- getEnvTagsWithPrefix "OPENTRACING_TAG_"
  case maybe_token_from_env of
    Just t -> do
      host <- fromMaybe "ingest.lightstep.com" <$> lookupOneOfEnvs ["LIGHTSTEP_HOST", "OPENTRACING_LIGHTSTEP_COLLECTOR_HOST"]
      port <- maybe 443 read <$> lookupOneOfEnvs ["LIGHTSTEP_PORT", "OPENTRACING_LIGHTSTEP_COLLECTOR_PORT"]
      service <- fromMaybe "example-haskell-service" <$> lookupOneOfEnvs ["LIGHTSTEP_SERVICE", "OPENTRACING_LIGHTSTEP_COMPONENT_NAME"]
      pure $ Just $ LightStepConfig host port (T.pack t) (T.pack service) global_tags 5
    Nothing -> do
      hPutStrLn stderr "LIGHTSTEP_ACCESS_TOKEN environment variable not defined"
      pure Nothing
