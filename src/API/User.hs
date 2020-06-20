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
                                                )
import           Servant

import           Config                         ( AppT(..) )
import           Model.User                     ( User(..)
                                                )


-- brittany-disable-next-binding
type UserAPI =
  BasicAuth "user-auth" (Entity User) :>
  "users"                    :>
  "authenticate"             :>
  Get '[JSON] (Maybe (Entity User))

userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = authenticate

-- The real authentication here is done through Basic Auth
-- The authCheck method located in src/API.hs will return
-- Unauthorized if authentication fails.
-- If Authentication succeeds we simply return the Entity User
authenticate :: MonadIO m => (Entity User) -> AppT m (Maybe (Entity User))
authenticate user = return $ Just user
