{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module API (app) where

import Control.Monad.Reader     (runReaderT)
import Servant                  (Proxy (Proxy)
                                ,Raw, Server, serve)

import Servant.Server
import API.Service              (ServiceAPI
                                ,serviceAPI
                                ,serviceServer)
import Config                   (AppT (..), Config (..))

serviceApp :: Config -> Application
serviceApp cfg = serve serviceAPI (appToServer cfg)

appToServer :: Config -> Server ServiceAPI
appToServer cfg = hoistServer serviceAPI (convertApp cfg) serviceServer

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

type AppAPI = ServiceAPI

appAPI :: Proxy AppAPI
appAPI = Proxy

app :: Config -> Application
app cfg =
    serve appAPI $ appToServer cfg
