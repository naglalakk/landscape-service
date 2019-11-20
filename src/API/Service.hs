{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}

module API.Service where

import Servant
import Control.Monad.IO.Class       (MonadIO)

import API.BlogPost                 (BlogPostAPI
                                    ,blogPostServer)
import API.Media                    (MediaAPI
                                    ,mediaServer)
import Config                       (AppT (..))

type ServiceAPI =  BlogPostAPI
              :<|> MediaAPI

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

serviceServer :: MonadIO m
              => ServerT ServiceAPI (AppT m)
serviceServer = blogPostServer :<|> mediaServer
