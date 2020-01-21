{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API (app) where

import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.IO.Class     (MonadIO
                                            ,liftIO)
import           Data.ByteString            as BS
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Servant
import           Servant.Server
import           Data.Time.Clock
import           Data.Time.Calendar

import API.Service                          (ServiceAPI
                                            ,serviceAPI
                                            ,serviceServer)
import Config                               (AppT(..), Config(..))
import Models

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck ::BasicAuthCheck User
authCheck = 
  let 
    day = fromGregorian (2020 :: Integer ) 1 1
    ts  = secondsToDiffTime (60000 :: Integer)
    defTs = UTCTime day ts
    check (BasicAuthData username password) =
        if username == "servant" && password == "server"
        then return (Authorized (User "servant" (TE.decodeUtf8 password) Nothing defTs Nothing))
        else return Unauthorized
  in BasicAuthCheck check

-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

appToServer :: Config 
            -> Server ServiceAPI
appToServer cfg = hoistServerWithContext serviceAPI (Proxy :: Proxy '[BasicAuthCheck User]) (convertApp cfg) serviceServer

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

files :: Server Raw
files = serveDirectoryFileServer "static"

type AppAPI = ServiceAPI :<|> "static" :> Raw

appAPI :: Proxy AppAPI
appAPI = Proxy

app :: Config 
    -> Application
app cfg = serveWithContext appAPI basicAuthServerContext (appToServer cfg :<|> files)
