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

import           Crypto.BCrypt              (hashPasswordUsingPolicy
                                            ,slowerBcryptHashingPolicy)
import           Control.Monad.Reader       (MonadIO
                                            ,MonadReader
                                            ,asks
                                            ,liftIO)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Aeson                 (FromJSON
                                            ,ToJSON
                                            ,(.:)
                                            ,(.:?)
                                            ,(.=)
                                            ,object
                                            ,parseJSON
                                            ,toJSON
                                            ,withObject)

import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TE
import           Data.Time                  (UTCTime
                                            ,getCurrentTime)
import           Database.Persist.Sql       (Entity(..)
                                            ,SqlPersistT
                                            ,(<-.)
                                            ,(=.)
                                            ,(==.)
                                            ,fromSqlKey
                                            ,runMigration
                                            ,runSqlPool
                                            ,selectFirst
                                            ,selectList
                                            ,insert)
import           Database.Persist.TH        (mkMigrate
                                            ,mkPersist
                                            ,persistLowerCase
                                            ,share
                                            ,sqlSettings)
import           GHC.Generics               (Generic)

import           Config                     (AppT
                                            ,Config
                                            ,configPool)

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
    slug            Text
    UniqueSlug      slug
    content         Text
    htmlContent     Text Maybe
    featuredImage   ImageId Maybe
    images          [ImageId]
    published       Bool
    publishTime     UTCTime
    showDate        Bool
    isCover         Bool
    createdAt       UTCTime
    updatedAt       UTCTime Maybe
    deriving Show Eq Generic

User 
    username        Text
    UniqueUsername  username
    password        Text
    email           Text Maybe
    isAdmin         Bool
    createdAt       UTCTime
    updatedAt       UTCTime Maybe
    deriving Show Eq Generic
|]

data BlogPostJSON =
    BlogPostJSON 
    (Entity BlogPost)           -- ^ The Blogpost
    (Maybe (Entity Image))      -- ^ Featured Image
    [Entity Image]              -- ^ BlogPost Image(s)

instance FromJSON BlogPost where
    parseJSON =
        withObject "blogPost" $ \b -> do
            BlogPost <$> 
                b .: "title"            <*> 
                b .: "slug"             <*>
                b .: "content"          <*> 
                b .:? "htmlContent"     <*>
                b .:? "featured_image"  <*>
                b .: "images"           <*>
                b .: "published"        <*>
                b .: "publish_time"     <*>
                b .: "show_date"        <*>
                b .: "is_cover"         <*>
                b .: "created_at"       <*>
                b .:? "updated_at"

instance ToJSON BlogPostJSON where
    toJSON (BlogPostJSON blogPost featuredImage images) =
        object
            [ "id" .= (fromSqlKey $ entityKey blogPost)
            , "title" .= (blogPostTitle $ entityVal blogPost)
            , "slug"  .= (blogPostSlug $ entityVal blogPost)
            , "content" .= (blogPostContent $ entityVal blogPost)
            , "htmlContent" .= (blogPostHtmlContent $ entityVal blogPost)
            , "featured_image" .= featuredImage
            , "images" .= images
            , "published" .= (blogPostPublished $ entityVal blogPost)
            , "publish_time" .= (blogPostPublishTime $ entityVal blogPost)
            , "show_date" .= (blogPostShowDate $ entityVal blogPost)
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

instance FromJSON User where
    parseJSON = 
        withObject "user" $ \u -> do
            User <$>
                u .:  "username"    <*>
                u .:  "password"    <*>
                u .:? "email"       <*>
                u .:  "is_admin"    <*>
                u .:  "created_at"  <*>
                u .:? "updated_at" 

instance ToJSON (Entity User) where
    toJSON (Entity userId (u@User {..})) =
        object
            [ "id" .= userId
            , "username"    .= userUsername
            , "email"       .= userEmail
            , "is_admin"    .= userIsAdmin
            , "created_at"  .= userCreatedAt
            , "updated_at"  .= userUpdatedAt ]


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

createUser :: Config 
           -> User
           -> IO (Maybe (Entity User))
createUser config user = do
    now <- getCurrentTime
    passw <- hashPasswordUsingPolicy 
             slowerBcryptHashingPolicy
             (TE.encodeUtf8 $ userPassword user) 
    case passw of
      Just p -> do
          let nUser = user { userPassword = TE.decodeUtf8 p
                           , userCreatedAt = now
                           }
          newUserId <- runReaderT (runDb $ insert nUser) config
          return $ Just $ Entity newUserId nUser
      Nothing -> return Nothing

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks configPool
    liftIO $ runSqlPool query pool
