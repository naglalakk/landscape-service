{-# LANGUAGE FlexibleContexts #-}

module Db where

import Config (Config (..))
import Control.Monad.Reader
  ( MonadIO,
    MonadReader,
    asks,
    liftIO,
  )
import Database.Persist.Sql
  ( SqlPersistT,
    runSqlPool,
  )

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool
