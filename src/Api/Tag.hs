{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Tag where

import Config (AppT (..))
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Data.Maybe (Maybe (..))
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Sql
  ( (==.),
    Entity (..),
    insert,
    selectFirst,
    toSqlKey,
  )
import Db (runDb)
import Model.Tag
  ( EntityField (TagLabel),
    EntityField (TagId),
    Tag (..),
    TagId,
  )
import Model.User (User (..))
import Servant

-- brittany-disable-next-binding
type TagAPI =
  BasicAuth "user-auth" (Entity User)
    :> "tags"
    :> Capture "tag" T.Text
    :> Post '[JSON] (Maybe (Entity Tag))
    :<|> "tags"
    :> Capture "tagId" Int
    :> Get '[JSON] (Maybe (Entity Tag))

tagServer :: MonadIO m => ServerT TagAPI (AppT m)
tagServer = createTag :<|> getTagById

createTag :: MonadIO m => (Entity User) -> T.Text -> AppT m (Maybe (Entity Tag))
createTag user tag = do
  hit <- runDb $ selectFirst [TagLabel ==. tag] []
  case hit of
    Just results -> return $ Just results
    Nothing -> do
      now <- liftIO getCurrentTime
      let newTag =
            Tag {tagLabel = tag, tagCreatedAt = now, tagUpdatedAt = Nothing}
      newTagId <- runDb $ insert newTag
      return $ Just $ Entity newTagId newTag

getTagById :: MonadIO m => Int -> AppT m (Maybe (Entity Tag))
getTagById tagId = runDb $ selectFirst [TagId ==. modelId] []
  where
    modelId = (toSqlKey $ fromIntegral tagId) :: TagId
