{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module API.Media where

import           Control.Applicative.Combinators
                                                ( optional )
import           Control.Monad.Except           ( MonadIO )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader           ( asks )
import           Data.Int                       ( Int64 )
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                )
import qualified Data.Text                     as T
import           Data.Time.Calendar             ( fromGregorian )
import           Data.Time.Clock                ( UTCTime(..)
                                                , getCurrentTime
                                                , secondsToDiffTime
                                                )
import           Data.UUID                      ( toString )
import           Data.UUID.V4                   ( nextRandom )
import           Database.Persist.Postgresql    ( Entity(..)
                                                , SelectOpt(..)
                                                , delete
                                                , insert
                                                , selectList
                                                , toSqlKey
                                                )
import           Servant
import           Servant.Multipart
import           System.Directory               ( doesFileExist
                                                , renameFile
                                                )
import           System.FilePath.Posix          ( takeDirectory
                                                , takeBaseName
                                                , takeExtensions
                                                , takeExtension
                                                , takeFileName
                                                )

import           Config                         ( AppT(..)
                                                , filePath
                                                , Config(..)
                                                )
import           Db                             ( runDb )
import           Model.Image                    ( Image(..)
                                                , ImageId 
                                                , EntityField ( ImageCreatedAt )
                                                , imageName
                                                , imageSrc
                                                , imageThumbnail
                                                , imageCreatedAt
                                                )
import           Model.User                     ( User )
import           Utils                          ( processImage )

instance FromMultipart Tmp Image where
  fromMultipart multipartData =
    Image
      <$> fmap fdFileName (lookupFile "image" multipartData)
      <*> fmap T.pack     (fmap fdPayload (lookupFile "image" multipartData))
      <*> (optional $ lookupInput "thumbnail" multipartData)
      <*> (Just $ UTCTime (fromGregorian 2019 1 1) (secondsToDiffTime 0))
      <*> (optional Nothing)


-- brittany-disable-next-binding
type MediaAPI =
  "media"                                  :>
  "images"                                 :>
  QueryParam "page" Int                    :>
  QueryParam "perPage" Int                 :>
  Get '[ JSON] [Entity Image]              :<|>
  MultipartForm Tmp Image                  :>
  BasicAuth "user-auth" (Entity User)      :>
  "media"                                  :>
  "images"                                 :>
  "upload"                                 :>
  Post '[JSON] (Maybe (Entity Image))      :<|>
  BasicAuth "user-auth" (Entity User)      :>
  "media"                                  :>
  "images"                                 :>
  Capture "imageId" Int                    :>
  "delete" :> Delete '[ JSON] ()

mediaServer :: MonadIO m => ServerT MediaAPI (AppT m)
mediaServer = allImages :<|> uploadImage :<|> deleteImage

toImageId :: Int64 -> ImageId
toImageId = toSqlKey

allImages :: MonadIO m => Maybe Int -> Maybe Int -> AppT m [Entity Image]
allImages page perPage = do
  let pageNr         = fromMaybe 1 page
      resultsPerPage = fromMaybe 10 perPage
      offset         = (pageNr - 1) * resultsPerPage
  runDb $ selectList
    []
    [Desc ImageCreatedAt, LimitTo resultsPerPage, OffsetBy offset]

uploadImage :: MonadIO m => Image -> (Entity User) -> AppT m (Maybe (Entity Image))
uploadImage img user = do
  environment <- asks configEnv
  -- Get currentTime for create UTCTime
  currentTime <- liftIO getCurrentTime
  -- Get full filename with static path
  let fileName = T.pack (filePath environment) <> imageName img
  -- Check if file already exists
  fileExists <- liftIO $ doesFileExist (T.unpack fileName)
  finalName  <- case fileExists of
    True -> do
      uuid <- liftIO nextRandom
      let uuidStr = toString uuid
          fnBase  = takeBaseName (T.unpack fileName)
          ext     = takeExtensions (T.unpack fileName)
          final   = T.pack $ (filePath environment) ++ fnBase ++ uuidStr ++ ext
      return final
    False -> return fileName
      --  If file exists we create a new path
      --  appending a unique uuid string to the filename
  -- Get only the filename
  let onlyName = T.pack (takeFileName $ T.unpack finalName)
  -- Rename the temporary upload file to final destination
  liftIO $ renameFile (T.unpack $ imageSrc img) ((filePath environment) <> T.unpack onlyName)
  -- process Image and create thumbnail
  let thumbnailPath =
        T.pack
          $  takeDirectory (T.unpack finalName)
          ++ "/"
          ++ (takeFileName $ T.unpack finalName)
          ++ "_thumbnail"
          ++ (takeExtension $ T.unpack finalName)
  liftIO $ processImage finalName thumbnailPath 400  400
  liftIO $ processImage finalName finalName     1100 700
  let image = img { imageName      = onlyName
                  , imageSrc       = finalName
                  , imageThumbnail = Just thumbnailPath
                  , imageCreatedAt = currentTime
                  }
  dbImage <- runDb $ insert image
  return $ Just $ Entity dbImage image

deleteImage :: MonadIO m => (Entity User) -> Int -> AppT m ()
deleteImage user imageId = runDb $ delete imageSqlKey
  where imageSqlKey = toImageId $ fromIntegral imageId
