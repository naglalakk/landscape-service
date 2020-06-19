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

module Model.Image where

import           Data.Aeson                     ( ToJSON
                                                , (.=)
                                                , object
                                                , toJSON
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


share [mkPersist sqlSettings, mkMigrate "migrateImage"]
    [persistLowerCase|

Image
    name            Text
    src             Text
    thumbnail       Text    Maybe
    createdAt       UTCTime
    updatedAt       UTCTime Maybe
    deriving Show Eq Generic
|]

instance ToJSON (Entity Image) where
  toJSON (Entity imgId (i@Image {..})) = object
    [ "id" .= imgId
    , "src" .= imageSrc
    , "thumbnail" .= imageThumbnail
    , "name" .= imageName
    , "created_at" .= imageCreatedAt
    , "updated_at" .= imageUpdatedAt
    ]
