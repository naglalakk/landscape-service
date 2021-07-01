{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exhibition where

import Config (AppT (..))
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Maybe
  ( Maybe (..),
    fromJust,
    fromMaybe,
    isJust,
  )
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Database.Bloodhound as BH
import Database.Persist.Sql
  ( (<-.),
    (=.),
    (==.),
    Entity (..),
    Filter (..),
    SelectOpt (..),
    delete,
    fromSqlKey,
    insert,
    selectFirst,
    selectList,
    toSqlKey,
    updateGet,
  )
import Db (runDb)
import Model.Exhibition
import Model.Item
import Model.User (User (..))
import Network.HTTP.Client (responseBody)
import Servant

-- /exhibitions/ (GET)
-- /exhibitions/<id>/
-- /exhibitions/<id>/items/
type ExhibitionUnprotecedAPI =
  "exhibitions"
    :> Get '[JSON] [ExhibitionJSON]
    :<|> "exhibitions"
    :> Capture "exhibitionId" Int
    :> Get '[JSON] (Maybe ExhibitionJSON)
    :<|> "exhibitions"
    :> Capture "exhibitionId" Int
    :> "items"
    :> Get '[JSON] [ItemJSON]

-- /exhibitions/ (POST)
-- /exhibitions/<id>/update
-- /exhibitions/<id>/delete
type ExhibitionProtectedAPI =
  "exhibitions"
    :> ReqBody '[JSON] Exhibition
    :> Post '[JSON] (Maybe ExhibitionJSON)
    :<|> "exhibitions"
    :> Capture "exhibitionId" Int
    :> "update"
    :> ReqBody '[JSON] Exhibition
    :> Post '[JSON] (Maybe ExhibitionJSON)
    :<|> "exhibitions"
    :> Capture "exhibitionId" Int
    :> "delete"
    :> Delete '[JSON] ()

type ExhibitionAPI =
  ExhibitionUnprotecedAPI
    :<|> BasicAuth "user-auth" (Entity User)
    :> ExhibitionProtectedAPI

exhibitionProtectedServer ::
  MonadIO m =>
  Entity User ->
  ServerT ExhibitionProtectedAPI (AppT m)
exhibitionProtectedServer (user :: (Entity User)) =
  createExhibition
    :<|> updateExhibition
    :<|> deleteExhibition

exhibitionUnprotectedServer ::
  MonadIO m =>
  ServerT ExhibitionUnprotecedAPI (AppT m)
exhibitionUnprotectedServer =
  allExhibitions
    :<|> getExhibition
    :<|> getExhibitionItems

exhibitionServer ::
  MonadIO m =>
  ServerT ExhibitionAPI (AppT m)
exhibitionServer =
  exhibitionUnprotectedServer
    :<|> exhibitionProtectedServer

-- | Create Exhibition
--   Endpoint: /exhibitions/
createExhibition ::
  MonadIO m =>
  Exhibition ->
  AppT m (Maybe ExhibitionJSON)
createExhibition ex = do
  now <- liftIO getCurrentTime
  newEx <- runDb $ insert ex {exhibitionCreatedAt = now}
  json <- exhibitionToExJSON $ Entity newEx ex
  return $ Just json

-- | Update Exhibition
--   Endpoint: /exhibitions/<id>/update
updateExhibition ::
  MonadIO m =>
  Int ->
  Exhibition ->
  AppT m (Maybe ExhibitionJSON)
updateExhibition exId ex = do
  now <- liftIO getCurrentTime
  updatedRec <-
    runDb $
      updateGet
        sqlKey
        [ ExhibitionTitle =. exhibitionTitle ex,
          ExhibitionFeaturedImage =. exhibitionFeaturedImage ex,
          ExhibitionIntroduction =. exhibitionIntroduction ex,
          ExhibitionItems =. exhibitionItems ex,
          ExhibitionStartDate =. exhibitionStartDate ex,
          ExhibitionEndDate =. exhibitionEndDate ex,
          ExhibitionUpdatedAt =. Just now
        ]
  json <- exhibitionToExJSON $ Entity sqlKey updatedRec
  return $ Just json
  where
    sqlKey = toSqlKey $ fromIntegral exId

-- | Delete Exhibition
--   Endpoint: /exhibitions/<id>/delete
deleteExhibition ::
  MonadIO m =>
  Int ->
  AppT m ()
deleteExhibition exId = runDb $ delete sqlKey
  where
    sqlKey = toSqlKey $ fromIntegral exId :: ExhibitionId

-- | Get all exhibitions
--   Endpoint: /exhibitions/
allExhibitions ::
  MonadIO m =>
  AppT m [ExhibitionJSON]
allExhibitions = do
  exhibitions <- runDb $ selectList ([] :: [Filter Exhibition]) []
  mapM exhibitionToExJSON exhibitions

-- | Get a single exhibition by id
--   Endpoint: /exhibition/<id>/
getExhibition ::
  MonadIO m =>
  Int ->
  AppT m (Maybe ExhibitionJSON)
getExhibition exId = do
  exDb <- runDb $ selectFirst [ExhibitionId ==. sqlKey] []
  case exDb of
    Just ex -> do
      json <- exhibitionToExJSON ex
      return $ Just json
    Nothing -> return Nothing
  where
    sqlKey = toSqlKey $ fromIntegral exId

-- | Get all items on exhibition
--   Endpoint: /exhibition/<id>/items/
getExhibitionItems :: MonadIO m => Int -> AppT m [ItemJSON]
getExhibitionItems exId = do
  exhibition <- runDb $ selectFirst [ExhibitionId ==. sqlKey] []
  case exhibition of
    Just (Entity exId exh) -> do
      items <- runDb $ selectList [ItemId <-. (exhibitionItems exh)] []
      json <- mapM itemToItemJSON items
      return json
    Nothing -> return []
  where
    sqlKey = toSqlKey $ fromIntegral exId
