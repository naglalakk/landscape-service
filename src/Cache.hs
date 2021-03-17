{-# LANGUAGE FlexibleContexts #-}

module Cache where

import Config
  ( Config,
    redisConnection,
  )
import Control.Monad.Reader
  ( MonadIO,
    MonadReader,
    asks,
    liftIO,
  )
import Database.Redis
  ( Redis,
    runRedis,
  )

runCache :: (MonadReader Config m, MonadIO m) => Redis a -> m a
runCache query = do
  conn <- asks redisConnection
  liftIO $ runRedis conn query
