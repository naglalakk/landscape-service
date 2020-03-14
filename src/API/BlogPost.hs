{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module API.BlogPost where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe             (Maybe(..), fromMaybe)
import qualified Data.Text              as T
import qualified Data.UUID              as UUID
import qualified Data.UUID.V4           as UUID
import           Data.Time.Clock        (getCurrentTime)
import           Database.Persist.Sql   (Entity(..)
                                        ,Filter(..)
                                        ,SelectOpt(..)
                                        ,(=.)
                                        ,(==.)
                                        ,delete
                                        ,fromSqlKey
                                        ,insert
                                        ,selectFirst
                                        ,selectList
                                        ,toSqlKey
                                        ,updateGet)
import           Servant
import           Servant.Server

import           Config                 (AppT(..))
import           Models                 (BlogPost(..)
                                        ,BlogPostId
                                        ,BlogPostJSON
                                        ,EntityField(..)
                                        ,User(..)
                                        ,blogPostToBlogPostJSON
                                        ,runDb)

type BlogPostUnprotecedAPI = "posts" :> 
                             QueryParam "page" Int :> 
                             QueryParam "perPage" Int :> 
                             Get '[JSON] [BlogPostJSON] 
                         :<|> "posts" :> 
                             Capture "postId" Int :> 
                             Get '[JSON] (Maybe BlogPostJSON) 
                        :<|> "posts" :>
                             Capture "slug" T.Text :>
                             Get '[JSON] (Maybe BlogPostJSON)

type BlogPostProtectedAPI = "posts" :> 
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

blogPostProtectedServer :: MonadIO m 
                        => User 
                        -> ServerT BlogPostProtectedAPI (AppT m)
blogPostProtectedServer (user :: User) =  createPost
                                     :<|> updatePost
                                     :<|> deletePost

blogPostUnprotectedServer :: MonadIO m 
                          => ServerT BlogPostUnprotecedAPI (AppT m)
blogPostUnprotectedServer = allPosts :<|> getBlogPost :<|> getBlogPostBySlug

type BlogPostAPI =  BlogPostUnprotecedAPI
               :<|> BasicAuth "user-auth" User :> BlogPostProtectedAPI 

blogPostServer :: MonadIO m => ServerT BlogPostAPI (AppT m)
blogPostServer =  blogPostUnprotectedServer
             :<|> blogPostProtectedServer

allPosts :: MonadIO m 
         => Maybe Int 
         -> Maybe Int 
         -> AppT m [BlogPostJSON]
allPosts page perPage = do
    let pageNr = fromMaybe 1 page
        resultsPerPage = fromMaybe 10 perPage
        offset = (pageNr - 1) * resultsPerPage
    posts <-
        runDb $
        selectList
            []
            [Desc BlogPostCreatedAt, LimitTo resultsPerPage, OffsetBy offset]
    json <- mapM blogPostToBlogPostJSON posts
    return json

getBlogPost :: MonadIO m 
            => Int 
            -> AppT m (Maybe BlogPostJSON)
getBlogPost postId = do
    post <- runDb $ selectFirst [BlogPostId ==. sqlKey] []
    case post of
        Just p -> do
            json <- blogPostToBlogPostJSON p
            return $ Just json
        Nothing -> return Nothing
  where
    sqlKey = (toSqlKey $ fromIntegral postId) :: BlogPostId

getBlogPostBySlug :: MonadIO m
                  => T.Text
                  -> AppT m (Maybe BlogPostJSON)
getBlogPostBySlug slug = do
    post <- runDb $ selectFirst [BlogPostSlug ==. slug ] []
    case post of
        Just p -> do
            json <- blogPostToBlogPostJSON p
            return $ Just json
        Nothing -> return Nothing

createSlug :: MonadIO m => T.Text -> AppT m T.Text
createSlug slug = do
    hit <- runDb $ selectFirst [ BlogPostSlug ==. slug ] []
    case hit of
      Just results -> do
          uuid <- liftIO $ UUID.toString <$> UUID.nextRandom
          let newSlug = (T.unpack slug) ++ uuid
          return $ T.pack newSlug
      Nothing      -> return slug

createPost :: MonadIO m => BlogPost -> AppT m (Maybe BlogPostJSON)
createPost blogPost = do
    slug    <- createSlug (blogPostSlug blogPost)
    newPost <- runDb $ insert $ blogPost { blogPostSlug = slug }
    json <- blogPostToBlogPostJSON $ Entity newPost blogPost
    return $ Just json

updatePost :: MonadIO m => Int -> BlogPost -> AppT m (Maybe BlogPostJSON)
updatePost postId post = do
    now <- liftIO getCurrentTime
    updatedRec <-
        runDb $
        updateGet
            sqlKey
            [ BlogPostTitle =. (blogPostTitle post)
            , BlogPostContent =. (blogPostContent post)
            , BlogPostHtmlContent =. (blogPostHtmlContent post)
            , BlogPostFeaturedImage =. (blogPostFeaturedImage post)
            , BlogPostImages =. (blogPostImages post)
            , BlogPostPublished =. (blogPostPublished post)
            , BlogPostPublishTime =. (blogPostPublishTime post)
            , BlogPostShowDate =. (blogPostShowDate post)
            , BlogPostIsCover =. (blogPostIsCover post)
            , BlogPostUpdatedAt =. Just now
            ]
    json <- blogPostToBlogPostJSON $ Entity sqlKey updatedRec
    return $ Just json
  where
    sqlKey = toSqlKey $ fromIntegral postId

deletePost :: MonadIO m => Int -> AppT m ()
deletePost postId = runDb $ delete sqlKey
  where
    sqlKey = (toSqlKey $ fromIntegral postId) :: BlogPostId
