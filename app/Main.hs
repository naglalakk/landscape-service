{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                  ( void )
import           Configuration.Dotenv           ( loadFile
                                                , defaultConfig
                                                )
import qualified Data.ByteString.UTF8          as BSU
import           Data.ByteString.Base64         ( encode )
import           Database.Persist.Postgresql    ( runSqlPool )
import qualified Data.Text                     as T
import           Data.Time                      ( getCurrentTime )
import           Network.Wai                    ( Middleware )
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.Cors
import           Options.Applicative
import qualified Network.HTTP.Types            as HTTP


import           API                            ( app )
import           Cli                            ( Command(..)
                                                , opts
                                                )
import           Config                         ( Environment(..)
                                                , getConfig
                                                , makePool
                                                )
import           Logger                         ( customLogger )
import           Models                         ( doMigrations )
import           Model.User                     ( User(..)
                                                , createUser
                                                )
import           Utils                          ( lookupSetting )

allowedMethods :: [HTTP.Method]
allowedMethods = ["GET", "HEAD", "POST", "PUT", "DELETE"]

-- | Allow Content-Type header with values other then allowed by simpleCors.
corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
 where
  policy = simpleCorsResourcePolicy
    { corsRequestHeaders = ["Authorization", "Content-Type"]
    , corsMethods        = allowedMethods
    }

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
  void $ loadFile defaultConfig
  cfg <- getConfig
  let opt = info
        (helper <*> opts)
        (fullDesc <> progDesc "Cli commands for donnabot.dev service" <> header
          "donnabot.dev cli"
        )
  command <- execParser opt
  case command of
    RunCommand portNr -> do
        -- Set up log file handler
      env  <- lookupSetting "ENV" Development
      pool <- makePool env

      -- Database migration
      runSqlPool doMigrations pool

      run portNr $ customLogger env $ corsWithContentType $ app cfg
    CreateUserCommand u p e a -> do
      now <- getCurrentTime
      let user = User username password email a now Nothing
      newUser <- createUser cfg user
      case newUser of
        Just usr -> do
          putStrLn $ "User with username: " ++ u ++ " created"
          case a of
            True -> do
              putStrLn $ "Your API key is: " ++ (show apiKey)
            False -> print ""
        Nothing -> putStrLn "failed creating a user"
     where
      username = T.pack u
      password = T.pack p
      email    = fmap T.pack e
      apiKey   = encode $ BSU.fromString $ u ++ ":" ++ p
