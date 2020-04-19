{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}

module API.User where

import           Control.Monad.Reader           ( MonadIO
                                                , MonadReader
                                                , liftIO
                                                , asks
                                                )
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Data.Time.Clock                ( getCurrentTime )
import           Database.Persist.Sql           ( Entity(..)
                                                , Filter(..)
                                                , SelectOpt(..)
                                                , (=.)
                                                , (==.)
                                                , delete
                                                , fromSqlKey
                                                , insert
                                                , selectFirst
                                                , selectList
                                                , toSqlKey
                                                , updateGet
                                                )
import           Servant
import           Servant.Server

import           Config                         ( AppT(..)
                                                , Config(..)
                                                )
import           Models                         ( EntityField(..)
                                                , User(..)
                                                , runDb
                                                )


-- brittany-disable-next-binding
type UserAPI =
  BasicAuth "user-auth" User :>
  "users"                    :>
  "authenticate"             :>
  Get '[JSON] (Maybe (Entity User))

userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = authenticate

authenticate :: MonadIO m => User -> AppT m (Maybe (Entity User))
authenticate user = do
  dbUser <- runDb $ selectFirst
    [UserUsername ==. (userUsername user), UserIsAdmin ==. True]
    []
  return dbUser

