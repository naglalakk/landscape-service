{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API
    ( app
    ) where

import Control.Monad.Reader (runReaderT)
import Servant
import Servant.Server

import API.Service (ServiceAPI, serviceAPI, serviceServer)
import Config (AppT(..), Config(..))

serviceApp :: Config -> Application
serviceApp cfg = serve serviceAPI (appToServer cfg)

appToServer :: Config -> Server ServiceAPI
appToServer cfg = hoistServer serviceAPI (convertApp cfg) serviceServer

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

files :: Server Raw
files = serveDirectoryFileServer "static"

type AppAPI = ServiceAPI :<|> "static" :> "uploads" :> Raw

appAPI :: Proxy AppAPI
appAPI = Proxy

app :: Config -> Application
app cfg = serve appAPI (appToServer cfg :<|> files)
