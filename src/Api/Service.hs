{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Api.Service where

import Api.BlogPost
  ( BlogPostAPI,
    blogPostServer,
  )
import Api.Media
  ( MediaAPI,
    mediaServer,
  )
import Api.Tag
  ( TagAPI,
    tagServer,
  )
import Api.User
  ( UserAPI,
    userServer,
  )
import Config (AppT (..))
import Control.Monad.Reader (MonadIO)
import Servant

type ServiceAPI =
  BlogPostAPI
    :<|> MediaAPI
    :<|> UserAPI
    :<|> TagAPI

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

serviceServer :: MonadIO m => ServerT ServiceAPI (AppT m)
serviceServer = blogPostServer :<|> mediaServer :<|> userServer :<|> tagServer
