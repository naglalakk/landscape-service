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

module Model.User where

import           Crypto.BCrypt                  ( hashPasswordUsingPolicy
                                                , slowerBcryptHashingPolicy
                                                )
import           Control.Monad.Trans.Reader     ( runReaderT )
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
import qualified Data.Text.Encoding            as TE
import           Data.Time                      ( UTCTime
                                                , getCurrentTime
                                                )
import           Database.Persist.Sql           ( Entity(..)
                                                , insert
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

share [mkPersist sqlSettings, mkMigrate "migrateUser"]
    [persistLowerCase|

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

instance FromJSON User where
  parseJSON = withObject "user" $ \u -> do
    User
      <$> u
      .:  "username"
      <*> u
      .:  "password"
      <*> u
      .:? "email"
      <*> u
      .:  "is_admin"
      <*> u
      .:  "created_at"
      <*> u
      .:? "updated_at"

instance FromJSON (Entity User) where
  parseJSON = withObject "user" $ \u -> do
    userId <- u .: "id"
    username <- u .: "username"
    let password = "<hidden>"
    email <- u .: "email"
    isAdmin <- u .: "is_admin"
    createdAt <- u .: "created_at"
    updatedAt <- u .:? "updated_at"
    let user = User username password email isAdmin createdAt updatedAt
    return $ (Entity userId user)

instance ToJSON (Entity User) where
  toJSON (Entity userId (u@User {..})) = object
    [ "id" .= userId
    , "username" .= userUsername
    , "email" .= userEmail
    , "is_admin" .= userIsAdmin
    , "created_at" .= userCreatedAt
    , "updated_at" .= userUpdatedAt
    ]

createUser :: Config -> User -> IO (Maybe (Entity User))
createUser config user = do
  now   <- getCurrentTime
  passw <- hashPasswordUsingPolicy slowerBcryptHashingPolicy
                                   (TE.encodeUtf8 $ userPassword user)
  case passw of
    Just p -> do
      let nUser = user { userPassword = TE.decodeUtf8 p, userCreatedAt = now }
      newUserId <- runReaderT (runDb $ insert nUser) config
      return $ Just $ Entity newUserId nUser
    Nothing -> return Nothing
