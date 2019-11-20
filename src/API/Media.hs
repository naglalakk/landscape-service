{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module API.Media where

import Servant
import Servant.Multipart
import qualified Data.Text              as T
import Control.Applicative.Combinators  (optional)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Except             (MonadIO)
import Data.Maybe                       (Maybe(..)
                                        ,fromMaybe)
import Database.Persist.Postgresql      (Entity(..)
                                        ,Filter(..)
                                        ,SelectOpt(..)
                                        ,fromSqlKey
                                        ,toSqlKey
                                        ,insert
                                        ,selectFirst
                                        ,selectList
                                        ,updateGet
                                        ,(=.)
                                        ,(==.))
import Data.Time.Clock                  (UTCTime(..)
                                        ,secondsToDiffTime
                                        ,getCurrentTime)
import Data.Time.Calendar               (fromGregorian)
import Data.UUID                        (toString)
import Data.UUID.V4                     (nextRandom)
import System.FilePath.Posix            (takeFileName
                                        ,takeExtensions
                                        ,takeBaseName)
import System.Directory                 (renameFile
                                        ,doesFileExist)

import Config                           (AppT(..), filePath)
import Models

instance FromMultipart Tmp Image where
    fromMultipart multipartData =
        Image <$> fmap fdFileName (lookupFile "image" multipartData)
              <*> fmap T.pack (fmap fdPayload (lookupFile "image" multipartData))
              <*> (optional $ lookupInput "thumbnail" multipartData)
              <*> (Just $ UTCTime (fromGregorian 2019 1 1) (secondsToDiffTime 0))
              <*> (optional Nothing)

type MediaAPI = "media" :>
                "images" :>
                QueryParam "page" Int :>
                QueryParam "perPage" Int :>
                Get '[JSON] [Entity Image]
            :<|>
                MultipartForm Tmp Image  :>
                "media" :>
                "images" :>
                "upload" :>
                Post '[JSON] (Maybe (Entity Image))

mediaServer :: MonadIO m => ServerT MediaAPI (AppT m)
mediaServer = allImages :<|> uploadImage

allImages :: MonadIO m
          => Maybe Int
          -> Maybe Int
          -> AppT m [Entity Image]
allImages page perPage = do
    let
        pageNr = fromMaybe 1 page
        resultsPerPage = fromMaybe 10 perPage
        offset = (pageNr - 1) * resultsPerPage
    runDb $ selectList [] [ Desc ImageCreatedAt
                          , LimitTo resultsPerPage
                          , OffsetBy offset ]

uploadImage :: MonadIO m => Image -> AppT m (Maybe (Entity Image))
uploadImage img = do
    currentTime <- liftIO getCurrentTime
    let fileName = T.pack filePath <> imageName img
    fileExists <- liftIO $ doesFileExist (T.unpack fileName)
    tmpFileExists <- liftIO $ doesFileExist (T.unpack $ imageSrc img)
    liftIO $ print tmpFileExists
    finalName <- case fileExists of
                    True -> do
                        uuid <- liftIO nextRandom
                        let uuidStr = toString uuid
                        let fnBase  = takeBaseName (T.unpack fileName)
                        let ext     = takeExtensions (T.unpack fileName)
                        return $ T.pack $ filePath ++ fnBase ++ uuidStr ++ ext
                    False -> return fileName
    tmpFileExists2 <- liftIO $ doesFileExist (T.unpack $ imageSrc img)
    let onlyName = T.pack (takeFileName $ T.unpack finalName)
    liftIO $ renameFile (T.unpack $ imageSrc img)  ("static/" <> T.unpack onlyName)
    let image = img {
         imageSrc = finalName
       , imageName = onlyName
       , imageCreatedAt = currentTime
    }
    dbImage <- runDb $ insert image
    return $ Just $ Entity dbImage img
