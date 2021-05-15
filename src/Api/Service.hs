{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Api.Service where

import Api.BlogPost
  ( BlogPostAPI,
    blogPostServer,
  )
import Api.Exhibition
  ( ExhibitionAPI,
    exhibitionServer,
  )
import Api.Item
  ( ItemAPI,
    itemServer,
  )
import Api.Media
  ( MediaAPI,
    mediaServer,
  )
import Api.Tag
  ( TagAPI,
    tagServer,
  )
import Api.Token
  ( TokenAPI,
    tokenServer,
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
    :<|> ExhibitionAPI
    :<|> ItemAPI
    :<|> TokenAPI

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

serviceServer ::
  MonadIO m =>
  ServerT ServiceAPI (AppT m)
serviceServer =
  blogPostServer
    :<|> mediaServer
    :<|> userServer
    :<|> tagServer
    :<|> exhibitionServer
    :<|> itemServer
    :<|> tokenServer
