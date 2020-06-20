{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API
  ( app
  )
where

import           Control.Monad.Reader           ( runReaderT )
import           Crypto.BCrypt                  ( validatePassword )
import           Data.Aeson
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text.Encoding            as TE
import           Database.Persist.Sql           ( Entity(..)
                                                , (==.)
                                                , runSqlPool
                                                , selectFirst
                                                )
import qualified Database.Redis                as Redis
import           Servant

import           API.Service                    ( ServiceAPI
                                                , serviceAPI
                                                , serviceServer
                                                )
import           Cache                          ( runCache )
import           Config                         ( AppT(..)
                                                , Config(..)
                                                , Environment(..)
                                                , getConfig
                                                )
import           Model.User                     ( User(..)
                                                , EntityField
                                                  ( UserIsAdmin
                                                  , UserUsername
                                                  )
                                                )

dbAuthCheck :: Config -> B.ByteString -> B.ByteString -> IO (Maybe (Entity User))
dbAuthCheck config username password = do
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
      if valid 
        then do
          -- Cache user
          let cacheStr = encode $ Entity userId user
          runReaderT (runCache $ do
            Redis.set username $ BL.toStrict cacheStr) config
          return $ Just $ Entity userId user
        else return Nothing
    Nothing -> return Nothing

authCheck :: BasicAuthCheck (Entity User)
authCheck =
  let
    check (BasicAuthData username password) = do
      config <- getConfig
      cachedUser <- runReaderT (runCache $ Redis.get username) config
      case cachedUser of
        Right usr -> do
          case usr of
            Just u -> do
              let decodedUser = decode $ BL.fromStrict u
              case decodedUser of
                Just entityUser -> return $ Authorized entityUser
                Nothing -> do
                  dbCheck <- dbAuthCheck config username password
                  case dbCheck of
                    Just authUser -> return $ Authorized authUser
                    Nothing -> return Unauthorized
            Nothing -> do
              dbCheck <- dbAuthCheck config username password
              case dbCheck of
                Just authUser -> return $ Authorized authUser
                Nothing -> return Unauthorized
        Left err -> do
          dbCheck <- dbAuthCheck config username password
          case dbCheck of
            Just authUser -> return $ Authorized authUser
            Nothing -> return Unauthorized
  in  BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck (Entity User) ': '[])
basicAuthServerContext = authCheck :. EmptyContext

appToServer :: Config -> Server ServiceAPI
appToServer cfg = hoistServerWithContext
  serviceAPI
  (Proxy :: Proxy '[BasicAuthCheck (Entity User)])
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
