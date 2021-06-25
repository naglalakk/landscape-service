{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Item where

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
import Model.Item
import Model.User (User (..))
import Network.HTTP.Client (responseBody)
import Servant

-- /items/ (GET)
-- /items/<id>/
-- /items/<id>/items
type ItemUnprotecedAPI =
  "items"
    :> Get '[JSON] [ItemJSON]
    :<|> "items"
    :> Capture "itemId" Int
    :> Get '[JSON] (Maybe ItemJSON)

-- /items/ (POST)
-- /items/<id>/update
-- /items/<id>/delete
type ItemProtectedAPI =
  "items"
    :> ReqBody '[JSON] Item
    :> Post '[JSON] (Maybe ItemJSON)
    :<|> "items"
    :> Capture "itemId" Int
    :> "update"
    :> ReqBody '[JSON] Item
    :> Post '[JSON] (Maybe ItemJSON)
    :<|> "items"
    :> Capture "itemId" Int
    :> "delete"
    :> Delete '[JSON] ()

type ItemAPI =
  ItemUnprotecedAPI
    :<|> BasicAuth "user-auth" (Entity User)
    :> ItemProtectedAPI

itemProtectedServer ::
  MonadIO m =>
  Entity User ->
  ServerT ItemProtectedAPI (AppT m)
itemProtectedServer (user :: (Entity User)) =
  createItem
    :<|> updateItem
    :<|> deleteItem

itemUnprotectedServer ::
  MonadIO m =>
  ServerT ItemUnprotecedAPI (AppT m)
itemUnprotectedServer =
  allItems
    :<|> getItem

itemServer ::
  MonadIO m =>
  ServerT ItemAPI (AppT m)
itemServer =
  itemUnprotectedServer
    :<|> itemProtectedServer

-- | Create Item
--   Endpoint: /items/
createItem ::
  MonadIO m =>
  Item ->
  AppT m (Maybe ItemJSON)
createItem item = do
  now <- liftIO getCurrentTime
  newItem <- runDb $ insert $ item {itemCreatedAt = now}
  json <- itemToItemJSON $ Entity newItem item
  return $ Just json

-- | Update Item
--   Endpoint: /items/<id>/update
updateItem ::
  MonadIO m =>
  Int ->
  Item ->
  AppT m (Maybe ItemJSON)
updateItem itemId item = do
  now <- liftIO getCurrentTime
  updatedRec <-
    runDb $
      updateGet
        sqlKey
        [ ItemTitle =. itemTitle item,
          ItemImage =. itemImage item,
          ItemToken =. itemToken item,
          ItemUpdatedAt =. Just now
        ]
  json <- itemToItemJSON $ Entity sqlKey updatedRec
  return $ Just json
  where
    sqlKey = toSqlKey $ fromIntegral itemId

-- | Delete Item
--   Endpoint: /items/<id>/delete
deleteItem ::
  MonadIO m =>
  Int ->
  AppT m ()
deleteItem itemId = runDb $ delete sqlKey
  where
    sqlKey = toSqlKey $ fromIntegral itemId :: ItemId

-- | Get all items
--   Endpoint: /items/
allItems ::
  MonadIO m =>
  AppT m [ItemJSON]
allItems = do
  items <- runDb $ selectList ([] :: [Filter Item]) []
  mapM itemToItemJSON items

-- | Get a single itemhibition by id
--   Endpoint: /items/<id>/
getItem ::
  MonadIO m =>
  Int ->
  AppT m (Maybe ItemJSON)
getItem itemId = do
  itemDb <- runDb $ selectFirst [ItemId ==. sqlKey] []
  case itemDb of
    Just item -> do
      json <- itemToItemJSON item
      return $ Just json
    Nothing -> return Nothing
  where
    sqlKey = toSqlKey $ fromIntegral itemId
