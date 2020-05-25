{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API
  ( app
  )
where

import           Control.Monad.Reader           ( runReaderT )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Crypto.BCrypt                  ( validatePassword )
import           Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Servant
import           Servant.Server
import           Data.Time.Clock
import           Data.Time.Calendar

import           Database.Persist.Sql           ( Entity(..)
                                                , (<-.)
                                                , (==.)
                                                , runSqlPool
                                                , selectFirst
                                                )

import           API.Service                    ( ServiceAPI
                                                , serviceAPI
                                                , serviceServer
                                                )
import           Config                         ( AppT(..)
                                                , Config(..)
                                                , Environment(..)
                                                , getConfig
                                                )
import           Models

authCheck :: BasicAuthCheck User
authCheck =
  let
    check (BasicAuthData username password) = do
      config <- getConfig
      user   <- runSqlPool
        (selectFirst
          [UserUsername ==. (TE.decodeUtf8 username), UserIsAdmin ==. True]
          []
        )
        (configPool config)
      case user of
        Just (Entity userId user) -> do
          let valid =
                validatePassword (TE.encodeUtf8 $ userPassword user) password
          if valid then return $ Authorized user else return Unauthorized
        Nothing -> return Unauthorized
  in  BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

appToServer :: Config -> Server ServiceAPI
appToServer cfg = hoistServerWithContext
  serviceAPI
  (Proxy :: Proxy '[BasicAuthCheck User])
  (convertApp cfg)
  serviceServer

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

files :: Environment -> Server Raw
files env = case env of
  Production  -> serveDirectoryFileServer "nofiles"
  Development -> serveDirectoryFileServer "static"

type AppAPI = ServiceAPI :<|> "static" :> Raw

appAPI :: Proxy AppAPI
appAPI = Proxy

app :: Config -> Application
app cfg = serveWithContext appAPI
                           basicAuthServerContext
                           (appToServer cfg :<|> (files $ configEnv cfg))
