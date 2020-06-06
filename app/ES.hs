{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Configuration.Dotenv           ( defaultConfig
                                                , loadFile
                                                )
import           Control.Monad
import           Control.Monad.Reader           ( runReaderT
                                                )
import           Data.Text                     as T
import           Database.Persist.Postgresql    ( Entity(..)
                                                , Filter(..)
                                                , fromSqlKey
                                                , selectList
                                                )
import           Database.Bloodhound     hiding ( Filter(..) )

import qualified Config                        as C
import           Elasticsearch
import           Models

getDocID :: BlogPostJSON -> T.Text
getDocID (BlogPostJSON blogPost fImage imgs) =
  T.pack $ show $ fromSqlKey $ entityKey blogPost

main :: IO ()
main = do
  void $ loadFile defaultConfig
  config <- C.getConfig
  let ixName = IndexName "donnabot-blogpost-index"
      mName  = MappingName "donnabot-blogpost-mapping"

  -- Clear the index
  runReaderT (runES $ destroyIndex $ ixName) config

  -- Recreate index
  index_   <- runReaderT (runES $ makeIndex $ ixName) config

  -- Create Blogpost mapping
  mapping_ <- runReaderT (runES $ makeMapping ixName mName BlogPostMapping)
                         config

  -- Get all Events
  records <- runReaderT (runDb $ selectList ([] :: [Filter BlogPost]) []) config

  -- Get Events JSON
  blogPosts <- runReaderT (mapM blogPostToBlogPostJSON records) config

  -- Update Blogpost or Create if it doesn't exist
  mapped <- runReaderT
    (mapM (\x -> updateOrCreate ixName mName (x) (getDocID x)) blogPosts)
    config

  putStrLn "ES sync completed..."
