{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}

module API.BlogPost where

import Servant
import Control.Monad.IO.Class       (MonadIO,liftIO)
import Data.Maybe                   (Maybe(..), fromMaybe)
import Data.Time.Clock              (getCurrentTime)
import Database.Persist.Sql         (Entity(..)
                                    ,Filter(..)
                                    ,SelectOpt(..)
                                    ,fromSqlKey
                                    ,toSqlKey
                                    ,selectFirst
                                    ,selectList
                                    ,insert
                                    ,updateGet
                                    ,delete
                                    ,(=.)
                                    ,(==.))

import Models                       (BlogPost(..)
                                    ,BlogPostJSON
                                    ,BlogPostId
                                    ,EntityField(..)
                                    ,runDb
                                    ,blogPostToBlogPostJSON)
import Config                       (AppT (..))

type BlogPostAPI = "posts" :>
                    QueryParam "page" Int :>
                    QueryParam "perPage" Int :>
                    Get '[JSON] [BlogPostJSON]
               :<|> "posts" :>
                    Capture "postId" Int :>
                    Get '[JSON] (Maybe BlogPostJSON)
               :<|> "posts" :>
                    ReqBody '[JSON] BlogPost :>
                    Post '[JSON] (Maybe BlogPostJSON)
               :<|> "posts" :>
                    Capture "postId" Int :>
                    "update" :>
                    ReqBody '[JSON] BlogPost :>
                    Post '[JSON] (Maybe BlogPostJSON)
               :<|> "posts" :>
                    Capture "postId" Int :>
                    "delete" :>
                    Delete '[JSON] ()

blogPostServer :: MonadIO m
               => ServerT BlogPostAPI (AppT m)
blogPostServer =  allPosts
             :<|> getBlogPost
             :<|> createPost
             :<|> updatePost
             :<|> deletePost

allPosts :: MonadIO m
         => Maybe Int
         -> Maybe Int
         -> AppT m [BlogPostJSON]
allPosts page perPage = do
    let
      pageNr = fromMaybe 1 page
      resultsPerPage = fromMaybe 10 perPage
      offset = (pageNr - 1) * resultsPerPage
    posts <- runDb $ selectList [] [ Desc BlogPostCreatedAt
                                   , LimitTo resultsPerPage
                                   , OffsetBy offset ]
    json <- mapM blogPostToBlogPostJSON posts
    return json

getBlogPost :: MonadIO m
            => Int
            -> AppT m (Maybe BlogPostJSON)
getBlogPost postId = do
    post <- runDb $ selectFirst [ BlogPostId ==. sqlKey ] []
    case post of
      Just p -> do
          json <- blogPostToBlogPostJSON p
          return $ Just json
      Nothing -> return Nothing

    where
        sqlKey = (toSqlKey $ fromIntegral postId) :: BlogPostId

createPost :: MonadIO m
           => BlogPost
           -> AppT m (Maybe BlogPostJSON)
createPost blogPost = do
    newPost <- runDb $ insert blogPost
    json    <- blogPostToBlogPostJSON $ Entity newPost blogPost
    return $ Just json

updatePost :: MonadIO m
           => Int
           -> BlogPost
           -> AppT m (Maybe BlogPostJSON)
updatePost postId post = do
    now <- liftIO getCurrentTime
    updatedRec <- runDb $ updateGet
        sqlKey
        [ BlogPostTitle =. (blogPostTitle post)
        , BlogPostContent =. (blogPostContent post)
        , BlogPostFeaturedImage =. (blogPostFeaturedImage post)
        , BlogPostPublishTime =. (blogPostPublishTime post)
        , BlogPostUpdatedAt =. Just now ]
    json <- blogPostToBlogPostJSON $ Entity sqlKey updatedRec
    return $ Just json
    where
        sqlKey = toSqlKey $ fromIntegral postId

deletePost :: MonadIO m
           => Int
           -> AppT m ()
deletePost postId = runDb $ delete sqlKey
    where
        sqlKey = (toSqlKey $ fromIntegral postId) :: BlogPostId
