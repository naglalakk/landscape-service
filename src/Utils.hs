{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Graphics.Image as Hip
import qualified Graphics.Image.IO as GIO
import qualified Graphics.Image.Interface.Vector as GIV
import qualified Graphics.Image.Processing as GIMP
import System.Environment (lookupEnv)
import System.FilePath.Posix
    ( FilePath
    , takeDirectory
    , takeExtension
    , takeFileName
    )

import Safe (readMay)

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing -> return def
        Just str -> maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $
        mconcat ["Failed to read [[", str, "]] for environment variable ", env]

-- Default processing of an image
-- Scales original image and rewrites
-- it at destination
-- creates a thumbnail for image
-- returns path to thumbnail
processImage :: T.Text -> IO T.Text
processImage path = do
    now <- getCurrentTime
    let realPath = T.replace "uploads/" "" path
    img <- Hip.readImageRGB GIV.VU $ T.unpack realPath
    let width = Hip.cols img
        height = Hip.rows img
        aspect = (fromIntegral width / fromIntegral height)
        thumbnailPath =
            takeDirectory (T.unpack realPath) ++
            "/" ++
            (takeFileName $ T.unpack realPath) ++
            "_thumbnail" ++ (takeExtension $ T.unpack realPath)
    (fullScale, thumbnailScale) <-
        case width > height of
            True -> do
                let newWidth = 1100
                    newHeight = newWidth / aspect
                    thumbnailWidth = 400
                    thumbnailHeight = thumbnailWidth / aspect
                return
                    ( (floor newHeight, floor newWidth)
                    , (floor thumbnailHeight, floor thumbnailWidth))
            False -> do
                let newHeight = 700
                    newWidth = newHeight * aspect
                    thumbnailHeight = 400
                    thumbnailWidth = thumbnailHeight * aspect
                return
                    ( (floor newHeight, floor newWidth)
                    , (floor thumbnailHeight, floor thumbnailWidth))
    Hip.writeImage (T.unpack realPath) $
        GIMP.resize GIMP.Bilinear GIMP.Edge fullScale img
    Hip.writeImage thumbnailPath $
        GIMP.resize GIMP.Bilinear GIMP.Edge thumbnailScale img
    return $ T.pack thumbnailPath
