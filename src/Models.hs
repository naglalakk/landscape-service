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

module Models where

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Data.Aeson
    ( FromJSON
    , ToJSON
    , (.:)
    , (.:?)
    , (.=)
    , object
    , parseJSON
    , toJSON
    , withObject
    )

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Sql
    ( Entity(..)
    , SqlPersistT
    , (<-.)
    , (=.)
    , (==.)
    , fromSqlKey
    , runMigration
    , runSqlPool
    , selectFirst
    , selectList
    )
import Database.Persist.TH
    ( mkMigrate
    , mkPersist
    , persistLowerCase
    , share
    , sqlSettings
    )
import GHC.Generics (Generic)

import Config (AppT, Config, configPool)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|

Image
    name            Text
    src             Text
    thumbnail       Text    Maybe
    createdAt       UTCTime
    updatedAt       UTCTime Maybe
    deriving Show Eq Generic

BlogPost
    title           Text
    content         Text
    htmlContent     Text Maybe
    featuredImage   ImageId Maybe
    images          [ImageId]
    published       Bool
    publishTime     UTCTime
    isCover         Bool
    createdAt       UTCTime
    updatedAt       UTCTime Maybe
    deriving Show Eq Generic
|]

data BlogPostJSON =
    BlogPostJSON (Entity BlogPost) (Maybe (Entity Image)) [Entity Image]

instance FromJSON BlogPost where
    parseJSON =
        withObject "blogPost" $ \b -> do
            BlogPost <$> b .: "title" <*> b .: "content" <*> b .:? "htmlContent" <*>
                b .:? "featured_image" <*>
                b .: "images" <*>
                b .: "published" <*>
                b .: "publish_time" <*>
                b .: "is_cover" <*>
                b .: "created_at" <*>
                b .:? "updated_at"

instance ToJSON BlogPostJSON where
    toJSON (BlogPostJSON blogPost featuredImage images) =
        object
            [ "id" .= (fromSqlKey $ entityKey blogPost)
            , "title" .= (blogPostTitle $ entityVal blogPost)
            , "content" .= (blogPostContent $ entityVal blogPost)
            , "htmlContent" .= (blogPostHtmlContent $ entityVal blogPost)
            , "featured_image" .= featuredImage
            , "images" .= images
            , "published" .= (blogPostPublished $ entityVal blogPost)
            , "publish_time" .= (blogPostPublishTime $ entityVal blogPost)
            , "is_cover" .= (blogPostIsCover $ entityVal blogPost)
            , "created_at" .= (blogPostCreatedAt $ entityVal blogPost)
            , "updated_at" .= (blogPostUpdatedAt $ entityVal blogPost)
            ]

instance ToJSON (Entity Image) where
    toJSON (Entity imgId (i@Image {..})) =
        object
            [ "id" .= imgId
            , "src" .= imageSrc
            , "thumbnail" .= imageThumbnail
            , "name" .= imageName
            , "created_at" .= imageCreatedAt
            , "updated_at" .= imageUpdatedAt
            ]

blogPostToBlogPostJSON :: MonadIO m => Entity BlogPost -> AppT m BlogPostJSON
blogPostToBlogPostJSON bp = do
    let eVal = entityVal bp
        fImg = blogPostFeaturedImage eVal
        imgs = blogPostImages eVal
    featuredImage <-
        case fImg of
            Just i -> runDb $ selectFirst [ImageId ==. i] []
            Nothing -> return Nothing
    images <- runDb $ selectList [ImageId <-. imgs] []
    return $ BlogPostJSON bp featuredImage images

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool
