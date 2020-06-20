{-# LANGUAGE FlexibleContexts           #-}

module Cache where

import           Database.Redis                 ( Redis
                                                , runRedis
                                                )
import           Control.Monad.Reader           ( MonadIO
                                                , MonadReader
                                                , asks
                                                , liftIO
                                                )

import           Config                         ( Config
                                                , redisConnection
                                                )

runCache :: (MonadReader Config m, MonadIO m) => Redis a -> m a
runCache query = do
  conn <- asks redisConnection
  liftIO $ runRedis conn query
