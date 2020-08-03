{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}

module API.Service where

import           Servant
import           Control.Monad.Reader           ( MonadIO )

import           API.BlogPost                   ( BlogPostAPI
                                                , blogPostServer
                                                )
import           API.Media                      ( MediaAPI
                                                , mediaServer
                                                )
import           API.User                       ( UserAPI
                                                , userServer
                                                )
import           API.Tag                        ( TagAPI
                                                , tagServer
                                                )
import           Config                         ( AppT(..) )

-- brittany-disable-next-binding
type ServiceAPI =
  BlogPostAPI :<|>
  MediaAPI    :<|>
  UserAPI     :<|>
  TagAPI

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

serviceServer :: MonadIO m => ServerT ServiceAPI (AppT m)
serviceServer = blogPostServer :<|> mediaServer :<|> userServer :<|> tagServer
