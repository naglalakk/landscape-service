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

module Model.Exhibition where

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
import Model.Item
import Model.Token

share
  [mkPersist sqlSettings, mkMigrate "migrateExhibition"]
  [persistLowerCase|

Exhibition json
  title Text
  featuredImage ImageId Maybe
  introduction Text Maybe
  items [ItemId]
  startDate UTCTime Maybe
  endDate UTCTime Maybe
  createdAt UTCTime
  updatedAt UTCTime Maybe
  deriving Eq Generic
|]

data ExhibitionJSON
  = ExhibitionJSON
      (Entity Exhibition)
      (Maybe (Entity Image))

instance ToJSON ExhibitionJSON where
  toJSON (ExhibitionJSON (Entity exId exhibition) featuredImage) =
    object
      [ "id" .= (fromSqlKey exId),
        "title" .= (exhibitionTitle exhibition),
        "featuredImage" .= featuredImage,
        "introduction" .= (exhibitionIntroduction exhibition),
        "items" .= (exhibitionItems exhibition),
        "startDate" .= (exhibitionStartDate exhibition),
        "endDate" .= (exhibitionEndDate exhibition),
        "createdAt" .= (exhibitionCreatedAt exhibition),
        "updatedAt" .= (exhibitionUpdatedAt exhibition)
      ]

exhibitionToExJSON ::
  (MonadReader Config m, MonadIO m) =>
  Entity Exhibition ->
  m ExhibitionJSON
exhibitionToExJSON (Entity exId exhibition) = do
  let fImg = exhibitionFeaturedImage exhibition
  featuredImage <- case fImg of
    Just i -> runDb $ selectFirst [ImageId ==. i] []
    Nothing -> return Nothing
  return $
    ExhibitionJSON
      (Entity exId exhibition)
      featuredImage
