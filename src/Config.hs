{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Exception              ( throwIO )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                )
import           Control.Monad.Logger           ( runStdoutLoggingT )
import           Control.Monad.Reader           ( MonadIO
                                                , MonadReader
                                                , ReaderT
                                                )
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe      ( MaybeT(..)
                                                , runMaybeT
                                                )
import qualified Data.ByteString.Char8         as BS
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( (<>) )
import           Database.Redis                 ( Connection
                                                , connect
                                                , connectHost
                                                , defaultConnectInfo
                                                )
import qualified Data.Text                     as T
import           Database.Persist.Postgresql    ( ConnectionPool
                                                , createPostgresqlPool
                                                )
import           Database.Bloodhound            ( BHEnv
                                                , Server(..)
                                                , mkBHEnv
                                                )
import           Network.HTTP.Client            ( defaultManagerSettings
                                                , newManager
                                                )
import           Network.Wai                    ( Middleware )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev )
import           Servant.Server                 ( ServerError )
import           System.Environment             ( lookupEnv )

import           Logger
import           Utils                          ( lookupSetting )

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype AppT m a =
    AppT
        { runApp :: ReaderT Config (ExceptT ServerError m) a
        }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Config
             , MonadError ServerError
             , MonadIO
             )

type App = AppT IO

-- | The Config for our application
data Config =
    Config
        -- Sql connection pool
        { configPool :: ConnectionPool
        -- Environment
        , configEnv  :: Environment
        -- Bloodhound (Elasticsearch) environment
        , esEnv      :: BHEnv
        -- Redis connection
        , redisConnection :: Connection
        }

-- | Right now, we're distinguishing
--   between three environments. We could
--   also add a @Staging@ environment if we needed to.
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

getConfig :: IO Config
getConfig = do
  env  <- lookupSetting "ENV" Development
  pool <- makePool env
  es   <- initES env
  cache <- initCache env
  return Config 
    { configPool = pool
    , configEnv = env
    , esEnv = es 
    , redisConnection = cache 
    }

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = apiFileLogger

makePool :: Environment -> IO ConnectionPool
makePool env = do
  pool <- runMaybeT $ do
    let keys = ["host=", " port=", " user=", " password=", " dbname="]
        envs = ["PGHOST", "PGPORT", "PGUSER", "PGPASS", "PGDATABASE"]
    envVars <- traverse (MaybeT . lookupEnv) envs
    let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
    lift $ runStdoutLoggingT $ createPostgresqlPool prodStr (envPool env)
  case pool of
    Nothing ->
      throwIO (userError "Database Configuration not present in environment.")
    Just a -> return a
      -- If we don't have a correct database configuration, we can't
      -- handle that in the program, so we throw an IO exception. This is
      -- one example where using an exception is preferable to 'Maybe' or
      -- 'Either'.

-- Initialize Redis Cache
initCache :: Environment -> IO Connection
initCache env = do
  case env of
    Production -> do
      host <- lookupEnv "REDIS_HOST"
      let connectInfo =
            defaultConnectInfo { connectHost = fromMaybe "localhost" host }
      connect connectInfo
    _ -> connect defaultConnectInfo

-- | Init Elasticsearch connection
initES :: Environment -> IO BHEnv
initES env = do
  manager <- newManager defaultManagerSettings
  host    <- lookupEnv "ES_URL"
  let serverStr = T.pack $ fromMaybe "http://localhost:9200" host
  return $ mkBHEnv (Server serverStr) manager

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

filePath :: Environment -> FilePath
filePath Development = "static/uploads/"
filePath Production  = "/var/www/donnabot/uploads/"
