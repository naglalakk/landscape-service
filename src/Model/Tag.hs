{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Tag where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , (.=)
                                                , (.:)
                                                , (.:?)
                                                , object
                                                , toJSON
                                                , parseJSON
                                                , withObject
                                                )

import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime
                                                )
import           Database.Persist.Sql           ( Entity(..)
                                                )
import           Database.Persist.TH            ( mkMigrate
                                                , mkPersist
                                                , persistLowerCase
                                                , share
                                                , sqlSettings
                                                )
import           GHC.Generics                   ( Generic )


share [mkPersist sqlSettings, mkMigrate "migrateTag"]
    [persistLowerCase|
Tag 
  label         Text
  UniqueLabel   label
  createdAt     UTCTime
  updatedAt     UTCTime Maybe
  deriving Show Eq Generic
|]

instance FromJSON Tag where
  parseJSON = withObject "tag" $ \t -> do
    Tag
      <$> t
      .:  "label"
      <*> t
      .:  "created_at"
      <*> t
      .:? "updated_at"

instance ToJSON (Entity Tag) where
  toJSON (Entity tagId (t@Tag {..})) = object
    [ "id" .= tagId
    , "label" .= tagLabel
    , "created_at" .= tagCreatedAt 
    , "updated_at" .= tagUpdatedAt
    ]
