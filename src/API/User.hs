{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}

module API.User where

import           Control.Monad.Reader           ( MonadIO )
import           Data.Maybe                     ( Maybe(..) )
import           Database.Persist.Sql           ( Entity(..)
                                                , (==.)
                                                , selectFirst
                                                )
import           Servant

import           Config                         ( AppT(..) )
import           Db                             ( runDb )
import           Model.User                     ( EntityField
                                                  ( UserIsAdmin
                                                  , UserUsername
                                                  )
                                                , User(..)
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

