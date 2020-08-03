{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module API.BlogPost where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Aeson
import qualified Database.Bloodhound           as BH
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                , fromJust
                                                , isJust
                                                )
import qualified Data.Text                     as T
import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID
import           Data.Time.Clock                ( getCurrentTime )
import           Database.Persist.Sql           ( Entity(..)
                                                , SelectOpt(..)
                                                , (=.)
                                                , (==.)
                                                , delete
                                                , insert
                                                , selectFirst
                                                , selectList
                                                , toSqlKey
                                                , fromSqlKey
                                                , updateGet
                                                )
import           Network.HTTP.Client            ( responseBody )
import           Servant

import           Config                         ( AppT(..) )
import           Db                             ( runDb )
import           Elasticsearch                  ( SearchQuery(..)
                                                , updateOrCreate
                                                , runES
                                                , blogPostIndexName
                                                , blogPostMappingName
                                                )
import           Model.BlogPost                 ( BlogPost(..)
                                                , BlogPostId
                                                , BlogPostJSON
                                                , EntityField(..)
                                                , blogPostToBlogPostJSON
                                                )
import           Model.User                     ( User(..) )

-- brittany-disable-next-binding
type BlogPostUnprotecedAPI =
  "posts"                               :>
  QueryParam "page" Int                 :>
  QueryParam "perPage" Int              :>
  Get '[JSON] [BlogPostJSON]            :<|>
  "posts"                               :>
  Capture "postId" Int                  :>
  Get '[JSON] (Maybe BlogPostJSON)      :<|>
  "posts"                               :>
  Capture "slug" T.Text                 :>
  Get '[JSON] (Maybe BlogPostJSON)      :<|>
  "posts"                               :>
  "search"                              :>
  ReqBody '[JSON] SearchQuery           :>
  Post '[JSON] (Maybe Value)

-- brittany-disable-next-binding
type BlogPostProtectedAPI =
  "posts"                                :>
  ReqBody '[JSON] BlogPost               :>
  Post '[JSON] (Maybe BlogPostJSON)      :<|>
  "posts"                                :>
  Capture "postId" Int                   :>
  "update"                               :>
  ReqBody '[JSON] BlogPost               :>
  Post '[JSON] (Maybe BlogPostJSON)      :<|>
  "posts"                                :>
  Capture "postId" Int                   :>
  "delete"                               :>
  Delete '[JSON] ()


-- brittany-disable-next-binding
type BlogPostAPI =
  BlogPostUnprotecedAPI               :<|>
  BasicAuth "user-auth" (Entity User) :>
  BlogPostProtectedAPI

blogPostProtectedServer
  :: MonadIO m => (Entity User) -> ServerT BlogPostProtectedAPI (AppT m)
blogPostProtectedServer (user :: (Entity User)) =
  createPost :<|> updatePost :<|> deletePost

blogPostUnprotectedServer :: MonadIO m => ServerT BlogPostUnprotecedAPI (AppT m)
blogPostUnprotectedServer =
  allPosts :<|> getBlogPost :<|> getBlogPostBySlug :<|> searchBlogPost

blogPostServer :: MonadIO m => ServerT BlogPostAPI (AppT m)
blogPostServer = blogPostUnprotectedServer :<|> blogPostProtectedServer

allPosts :: MonadIO m => Maybe Int -> Maybe Int -> AppT m [BlogPostJSON]
allPosts page perPage = do
  let pageNr         = fromMaybe 1 page
      resultsPerPage = fromMaybe 10 perPage
      offset         = (pageNr - 1) * resultsPerPage
  posts <- runDb $ selectList
    []
    [Desc BlogPostCreatedAt, LimitTo resultsPerPage, OffsetBy offset]
  json <- mapM blogPostToBlogPostJSON posts
  return json

getBlogPost :: MonadIO m => Int -> AppT m (Maybe BlogPostJSON)
getBlogPost postId = do
  post <- runDb $ selectFirst [BlogPostId ==. sqlKey] []
  case post of
    Just p -> do
      json <- blogPostToBlogPostJSON p
      return $ Just json
    Nothing -> return Nothing
  where sqlKey = (toSqlKey $ fromIntegral postId) :: BlogPostId

getBlogPostBySlug :: MonadIO m => T.Text -> AppT m (Maybe BlogPostJSON)
getBlogPostBySlug slug = do
  post <- runDb $ selectFirst [BlogPostSlug ==. slug] []
  case post of
    Just p -> do
      json <- blogPostToBlogPostJSON p
      return $ Just json
    Nothing -> return Nothing

createSlug :: MonadIO m => T.Text -> AppT m T.Text
createSlug slug = do
  hit <- runDb $ selectFirst [BlogPostSlug ==. slug] []
  case hit of
    Just results -> do
      uuid <- liftIO $ UUID.toString <$> UUID.nextRandom
      let newSlug = (T.unpack slug) ++ uuid
      return $ T.pack newSlug
    Nothing -> return slug

createPost :: MonadIO m => BlogPost -> AppT m (Maybe BlogPostJSON)
createPost blogPost = do
  slug    <- createSlug (blogPostSlug blogPost)
  newPost <- runDb $ insert $ blogPost { blogPostSlug = slug }
  json    <- blogPostToBlogPostJSON $ Entity newPost blogPost
  updateOrCreate blogPostIndexName
                 blogPostMappingName
                 json
                 (T.pack $ show $ fromSqlKey newPost)
  return $ Just json

updatePost :: MonadIO m => Int -> BlogPost -> AppT m (Maybe BlogPostJSON)
updatePost postId post = do
  now        <- liftIO getCurrentTime
  updatedRec <- runDb $ updateGet
    sqlKey
    [ BlogPostTitle =. (blogPostTitle post)
    , BlogPostContent =. (blogPostContent post)
    , BlogPostHtmlContent =. (blogPostHtmlContent post)
    , BlogPostFeaturedImage =. (blogPostFeaturedImage post)
    , BlogPostImages =. (blogPostImages post)
    , BlogPostTags   =.  (blogPostTags post)
    , BlogPostPublished =. (blogPostPublished post)
    , BlogPostPublishTime =. (blogPostPublishTime post)
    , BlogPostShowDate =. (blogPostShowDate post)
    , BlogPostIsCover =. (blogPostIsCover post)
    , BlogPostUpdatedAt =. Just now
    ]
  json <- blogPostToBlogPostJSON $ Entity sqlKey updatedRec
  updateOrCreate blogPostIndexName
                 blogPostMappingName
                 json
                 (T.pack $ show postId)
  return $ Just json
  where sqlKey = toSqlKey $ fromIntegral postId

deletePost :: MonadIO m => Int -> AppT m ()
deletePost postId = do
  runDb $ delete sqlKey
  runES $ BH.deleteDocument 
          blogPostIndexName
          blogPostMappingName
          (BH.DocId $ T.pack $ show postId)
  return ()
  where sqlKey = (toSqlKey $ fromIntegral postId) :: BlogPostId

searchBlogPost :: MonadIO m => SearchQuery -> AppT m (Maybe Value)
searchBlogPost query = do
  res <- runES $ BH.searchByType (BH.IndexName "donnabot-blogpost-index")
                                 (BH.MappingName "donnabot-blogpost-mapping")
                                 search
  let body     = decode (responseBody res) :: Maybe Object
      response = case body of
        Just b  -> HM.lookup "hits" b
        Nothing -> Nothing
  return response
 where
  boost       = Nothing
  per         = fromMaybe 25 $ perPage query
  pg          = (-) (fromMaybe 1 $ page query) 1
  justSParams = Prelude.map (\(x, y) -> (x, fromJust y))
                            (Prelude.filter (isJust . snd) (queries query))
  sQueries = Prelude.map
    (\(x, y) ->
      (BH.QueryMatchQuery $ BH.mkMatchQuery (BH.FieldName x) (BH.QueryString y))
    )
    justSParams
  sQuery = BH.QueryBoolQuery $ BH.mkBoolQuery sQueries [] [] []
  search = (BH.mkSearch (Just sQuery) boost)
    { BH.size     = BH.Size per
    , BH.from     = BH.From $ pg * per
    , BH.sortBody = Just
      [ BH.DefaultSortSpec
          $ (BH.mkSort (BH.FieldName "publish_time") (BH.Descending))
              { BH.ignoreUnmapped = (Just ("true" :: T.Text))
              }
      ]
    }
