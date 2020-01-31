{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad                            (void)
import Configuration.Dotenv                     (loadFile, defaultConfig)
import Control.Monad.Reader                     (runReaderT)
import Database.Persist.Postgresql              (runSqlPool)
import Network.Wai                              (Middleware)
import Network.Wai.Handler.Warp                 (run)
import Network.Wai.Middleware.Cors
import qualified Network.HTTP.Types             as HTTP
import Safe                                     (readMay)
import System.Environment                       (lookupEnv)
import System.Log.Logger
import System.Log.Handler                       (setFormatter)
import System.Log.Handler.Simple
import System.Log.Formatter


import API                                      (app)
import Config                                   (Config (..), Environment (..),
                                                makePool, setLogger, initES)
import Models                                   (doMigrations)
import Utils                                    (lookupSetting)

allowedMethods :: [HTTP.Method]
allowedMethods =
    [ "GET"
    , "HEAD"
    , "POST"
    , "PUT"
    , "DELETE"
    ]

-- | Allow Content-Type header with values other then allowed by simpleCors.
corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Authorization"
                               ,"Content-Type"
                               ] 
        , corsMethods = allowedMethods
        }

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
    void $ loadFile defaultConfig

    -- Set up log file handler
    updateGlobalLogger "BlogService"
                       (setLevel DEBUG)
    h <- fileHandler "logs/server.log" DEBUG >>= \lh -> return $
         setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "BlogService" (addHandler h)

    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORTNR" 8081
    esEnv <- initES env
    pool <- makePool env
    salt <- lookupSetting "SALT" ""
    let cfg = Config { configPool = pool
                     , configEnv = env
                     , esEnv = esEnv 
                     , saltKey = salt
                     }
        logger = setLogger env

    -- Database migration
    runSqlPool doMigrations pool

    run port $ logger $ corsWithContentType $ app cfg
