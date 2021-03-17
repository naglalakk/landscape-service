{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Config (AppT (..))
import Control.Monad.Reader (MonadIO)
import Data.Maybe (Maybe (..))
import Database.Persist.Sql
  ( Entity (..),
  )
import Model.User
  ( User (..),
  )
import Servant

type UserAPI =
  BasicAuth "user-auth" (Entity User)
    :> "users"
    :> "authenticate"
    :> Get '[JSON] (Maybe (Entity User))

userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = authenticate

-- The real authentication here is done through Basic Auth
-- The authCheck method located in src/API.hs will return
-- Unauthorized if authentication fails.
-- If Authentication succeeds we simply return the Entity User
authenticate :: MonadIO m => Entity User -> AppT m (Maybe (Entity User))
authenticate user = return $ Just user
