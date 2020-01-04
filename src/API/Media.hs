{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module API.Media where

import Control.Applicative.Combinators (optional)
import Control.Monad.Except (MonadIO)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Maybe (Maybe(..), fromMaybe)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), getCurrentTime, secondsToDiffTime)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.Persist.Postgresql
    ( Entity(..)
    , Filter(..)
    , SelectOpt(..)
    , (=.)
    , (==.)
    , delete
    , fromSqlKey
    , insert
    , selectFirst
    , selectList
    , toSqlKey
    , updateGet
    )
import Servant
import Servant.Multipart
import System.Directory (doesFileExist, renameFile)
import System.FilePath.Posix (takeBaseName, takeExtensions, takeFileName)

import Config (AppT(..), filePath)
import Models
import Utils (processImage)

instance FromMultipart Tmp Image where
    fromMultipart multipartData =
        Image <$> fmap fdFileName (lookupFile "image" multipartData) <*>
        fmap T.pack (fmap fdPayload (lookupFile "image" multipartData)) <*>
        (optional $ lookupInput "thumbnail" multipartData) <*>
        (Just $ UTCTime (fromGregorian 2019 1 1) (secondsToDiffTime 0)) <*>
        (optional Nothing)

type MediaAPI
     = "media" :> "images" :> QueryParam "page" Int :> QueryParam "perPage" Int :> Get '[ JSON] [Entity Image] :<|> MultipartForm Tmp Image :> "media" :> "images" :> "upload" :> Post '[ JSON] (Maybe (Entity Image)) :<|> "media" :> "images" :> Capture "imageId" Int :> "delete" :> Delete '[ JSON] ()

mediaServer :: MonadIO m => ServerT MediaAPI (AppT m)
mediaServer = allImages :<|> uploadImage :<|> deleteImage

toImageId :: Int64 -> ImageId
toImageId = toSqlKey

allImages :: MonadIO m => Maybe Int -> Maybe Int -> AppT m [Entity Image]
allImages page perPage = do
    let pageNr = fromMaybe 1 page
        resultsPerPage = fromMaybe 10 perPage
        offset = (pageNr - 1) * resultsPerPage
    runDb $
        selectList
            []
            [Desc ImageCreatedAt, LimitTo resultsPerPage, OffsetBy offset]

uploadImage :: MonadIO m => Image -> AppT m (Maybe (Entity Image))
uploadImage img = do
    currentTime <- liftIO getCurrentTime
    let fileName = T.pack filePath <> imageName img
    fileExists <- liftIO $ doesFileExist (T.unpack fileName)
    tmpFileExists <- liftIO $ doesFileExist (T.unpack $ imageSrc img)
    liftIO $ print tmpFileExists
    finalName <-
        case fileExists of
            True -> do
                uuid <- liftIO nextRandom
                let uuidStr = toString uuid
                let fnBase = takeBaseName (T.unpack fileName)
                let ext = takeExtensions (T.unpack fileName)
                return $ T.pack $ filePath ++ fnBase ++ uuidStr ++ ext
            False -> return fileName
    tmpFileExists2 <- liftIO $ doesFileExist (T.unpack $ imageSrc img)
    let onlyName = T.pack (takeFileName $ T.unpack finalName)
    liftIO $
        renameFile (T.unpack $ imageSrc img) ("static/" <> T.unpack onlyName)
    -- process Image and create thumbnail
    thumbnail <- liftIO $ processImage finalName
    liftIO $ print thumbnail
    let finalThumb = T.replace "static/" "static/uploads/" thumbnail
    let image =
            img
                { imageSrc = finalName
                , imageName = onlyName
                , imageThumbnail = Just finalThumb
                , imageCreatedAt = currentTime
                }
    dbImage <- runDb $ insert image
    return $ Just $ Entity dbImage img

deleteImage :: MonadIO m => Int -> AppT m ()
deleteImage imageId = runDb $ delete imageSqlKey
  where
    imageSqlKey = toImageId $ fromIntegral imageId
