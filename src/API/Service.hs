{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}

module API.Service where

import           Servant
import           Control.Monad.Reader           ( MonadIO
                                                , MonadReader
                                                )

import           API.BlogPost                   ( BlogPostAPI
                                                , blogPostServer
                                                )
import           API.Media                      ( MediaAPI
                                                , mediaServer
                                                )
import           API.User                       ( UserAPI
                                                , userServer
                                                )
import           Config                         ( AppT(..)
                                                , Config(..)
                                                )

-- brittany-disable-next-binding
type ServiceAPI =
  BlogPostAPI :<|>
  MediaAPI    :<|>
  UserAPI

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

serviceServer :: MonadIO m => ServerT ServiceAPI (AppT m)
serviceServer = blogPostServer :<|> mediaServer :<|> userServer
