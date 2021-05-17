{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Item where

import Config (Config)
import Control.Monad.Reader
  ( MonadIO,
    MonadReader,
  )
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson
  ( (.:),
    (.:?),
    (.=),
    FromJSON,
    ToJSON,
    object,
    parseJSON,
    toJSON,
    withObject,
  )
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
  ( UTCTime,
    getCurrentTime,
  )
import Database.Persist.Sql
  ( (<-.),
    (==.),
    Entity (..),
    fromSqlKey,
    insert,
    selectFirst,
    selectList,
  )
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Db (runDb)
import GHC.Generics (Generic)
import Model.Image
import Model.Token

share
  [mkPersist sqlSettings, mkMigrate "migrateItem"]
  [persistLowerCase|

Item json
  title Text
  image ImageId Maybe
  token TokenId Maybe
  createdAt UTCTime
  updatedAt UTCTime Maybe
  deriving Eq Generic
|]

data ItemJSON
  = ItemJSON
      (Entity Item)
      (Maybe (Entity Image))
      (Maybe (Entity Token))

instance ToJSON ItemJSON where
  toJSON (ItemJSON (Entity itemId item) image token) =
    object
      [ "id" .= (fromSqlKey itemId),
        "title" .= (itemTitle item),
        "image" .= image,
        "token" .= token,
        "createdAt" .= (itemCreatedAt item),
        "updatedAt" .= (itemUpdatedAt item)
      ]

itemToItemJSON :: (MonadReader Config m, MonadIO m) => Entity Item -> m ItemJSON
itemToItemJSON (Entity iId item) = do
  let img = itemImage item
      tok = itemToken item
  image <- case img of
    Just i -> runDb $ selectFirst [ImageId ==. i] []
    Nothing -> return Nothing
  token <- case tok of
    Just t -> runDb $ selectFirst [TokenId ==. t] []
    Nothing -> return Nothing
  return $ ItemJSON (Entity iId item) image token
