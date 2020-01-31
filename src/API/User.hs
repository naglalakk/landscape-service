{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module API.User where

import           Control.Monad.Reader   (MonadIO
                                        ,MonadReader
                                        ,liftIO
                                        ,asks)
import           Crypto.BCrypt          (hashPassword)
import           Data.Maybe             (Maybe(..)
                                        ,fromMaybe)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
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
import Servant
import Servant.Server

import Config                           (AppT(..)
                                        ,Config(..))
import Models                           (EntityField(..)
                                        ,User(..)
                                        ,runDb)

type UserAPI = "users" :>
               ReqBody '[JSON] User :>
               Post '[JSON] (Maybe (Entity User))
               
userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = createUser

createUser :: MonadIO m
           => User
           -> AppT m (Maybe (Entity User))
createUser user = do
    now <- liftIO getCurrentTime
    salt <- asks saltKey
    let 
      passw = hashPassword 
              (TE.encodeUtf8 $ userPassword user) 
              (TE.encodeUtf8 salt)
    case passw of
      Just p -> do
          let nUser = user { userPassword = TE.decodeUtf8 p
                           , userCreatedAt = now
                           }
          newUserId <- runDb $ insert nUser
          return $ Just $ Entity newUserId nUser
      Nothing -> return Nothing
