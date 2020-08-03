{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Configuration.Dotenv           ( defaultConfig
                                                , loadFile
                                                )
import           Control.Monad
import           Control.Monad.Reader           ( runReaderT )
import           Data.Text                     as T
import           Database.Persist.Postgresql    ( Entity(..)
                                                , Filter(..)
                                                , fromSqlKey
                                                , selectList
                                                )

import qualified Config                        as C
import           Db                             ( runDb )
import           Elasticsearch
import           Model.BlogPost                 ( BlogPost
                                                , BlogPostJSON(..)
                                                , blogPostToBlogPostJSON
                                                )

getDocID :: BlogPostJSON -> T.Text
getDocID (BlogPostJSON blogPost fImage imgs tags) =
  T.pack $ show $ fromSqlKey $ entityKey blogPost

main :: IO ()
main = do
  void $ loadFile defaultConfig
  config <- C.getConfig

  -- Clear the index
  runReaderT (runES $ destroyIndex $ blogPostIndexName) config

  -- Recreate index
  index_   <- runReaderT (runES $ makeIndex $ blogPostIndexName) config

  -- Create Blogpost mapping
  mapping_ <- runReaderT (runES $ makeMapping blogPostIndexName blogPostMappingName BlogPostMapping)
                         config

  -- Get all Events
  records <- runReaderT (runDb $ selectList ([] :: [Filter BlogPost]) []) config

  -- Get Events JSON
  blogPosts <- runReaderT (mapM blogPostToBlogPostJSON records) config

  -- Update Blogpost or Create if it doesn't exist
  mapped <- runReaderT
    (mapM (\x -> updateOrCreate blogPostIndexName blogPostMappingName (x) (getDocID x)) blogPosts)
    config

  putStrLn "ES sync completed..."
