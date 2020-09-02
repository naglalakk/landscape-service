{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
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

module Model.BlogPost where

import           Control.Monad.Reader           ( MonadIO
                                                , MonadReader
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , (.:)
                                                , (.:?)
                                                , (.=)
                                                , object
                                                , parseJSON
                                                , toJSON
                                                , withObject
                                                )

import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Database.Persist.Sql           ( Entity(..)
                                                , (<-.)
                                                , (==.)
                                                , fromSqlKey
                                                , selectFirst
                                                , selectList
                                                )
import           Database.Persist.TH            ( mkMigrate
                                                , mkPersist
                                                , persistLowerCase
                                                , share
                                                , sqlSettings
                                                )
import           GHC.Generics                   ( Generic )

import           Config                         ( Config )
import           Db                             ( runDb )
import           Model.Image                    ( Image
                                                , ImageId
                                                , EntityField(ImageId)
                                                )
import           Model.Tag                      ( Tag
                                                , TagId
                                                , EntityField(TagId)
                                                )

share [mkPersist sqlSettings, mkMigrate "migrateBlogPost"]
    [persistLowerCase|

BlogPost
    title           Text
    slug            Text
    UniqueSlug      slug
    content         Text
    htmlContent     Text Maybe
    featuredImage   ImageId Maybe
    images          [ImageId]
    tags            [TagId]
    published       Bool
    publishTime     UTCTime
    showDate        Bool
    isCover         Bool
    createdAt       UTCTime
    updatedAt       UTCTime Maybe
    deriving Show Eq Generic
|]

data BlogPostJSON =
    BlogPostJSON
    (Entity BlogPost)           -- The Blogpost
    (Maybe (Entity Image))      -- Featured Image
    [Entity Image]              -- BlogPost Image(s)
    [Entity Tag]                -- Tags

instance FromJSON BlogPost where
  parseJSON = withObject "blogPost" $ \b -> do
    BlogPost
      <$> b
      .:  "title"
      <*> b
      .:  "slug"
      <*> b
      .:  "content"
      <*> b
      .:? "htmlContent"
      <*> b
      .:? "featured_image"
      <*> b
      .:  "images"
      <*> b
      .: "tags"
      <*> b
      .:  "published"
      <*> b
      .:  "publish_time"
      <*> b
      .:  "show_date"
      <*> b
      .:  "is_cover"
      <*> b
      .:  "created_at"
      <*> b
      .:? "updated_at"

instance ToJSON BlogPostJSON where
  toJSON (BlogPostJSON blogPost featuredImage images tags) = object
    [ "id" .= (fromSqlKey $ entityKey blogPost)
    , "title" .= (blogPostTitle $ entityVal blogPost)
    , "slug" .= (blogPostSlug $ entityVal blogPost)
    , "content" .= (blogPostContent $ entityVal blogPost)
    , "htmlContent" .= (blogPostHtmlContent $ entityVal blogPost)
    , "featured_image" .= featuredImage
    , "images" .= images
    , "tags" .= tags
    , "published" .= (blogPostPublished $ entityVal blogPost)
    , "publish_time" .= (blogPostPublishTime $ entityVal blogPost)
    , "show_date" .= (blogPostShowDate $ entityVal blogPost)
    , "is_cover" .= (blogPostIsCover $ entityVal blogPost)
    , "created_at" .= (blogPostCreatedAt $ entityVal blogPost)
    , "updated_at" .= (blogPostUpdatedAt $ entityVal blogPost)
    ]

blogPostToBlogPostJSON
  :: (MonadReader Config m, MonadIO m) => Entity BlogPost -> m BlogPostJSON
blogPostToBlogPostJSON bp = do
  let eVal = entityVal bp
      fImg = blogPostFeaturedImage eVal
      imgs = blogPostImages eVal
      tgs  = blogPostTags eVal
  featuredImage <- case fImg of
    Just i  -> runDb $ selectFirst [ImageId ==. i] []
    Nothing -> return Nothing
  images <- runDb $ selectList [ImageId <-. imgs] []
  tags <- runDb $ selectList [TagId <-. tgs ] []
  return $ BlogPostJSON bp featuredImage images tags
