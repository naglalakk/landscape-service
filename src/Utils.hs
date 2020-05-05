{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}

module Utils where

import qualified Data.Text                     as T
import           Data.Time                      ( getCurrentTime )
import           Graphics.ImageMagick.MagickWand
import           System.Environment             ( lookupEnv )
import           System.FilePath.Posix          ( FilePath
                                                , takeDirectory
                                                , takeExtension
                                                , takeFileName
                                                )
import           Safe                           ( readMay )

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing  -> return def
    Just str -> maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $ mconcat
        ["Failed to read [[", str, "]] for environment variable ", env]

getScaledSizes
  :: Int -- current width of image
  -> Int -- current height of image
  -> Int -- desired width of scaling
  -> Int -- desired height of scaling
  -> (Int, Int) -- (width, height)
getScaledSizes currentWidth currentHeight maxWidth maxHeight = case resize of
  True -> case isLandscape of
    True  -> (maxWidth, floor ((fromIntegral maxWidth) / aspect))
    False -> (floor ((fromIntegral maxWidth) * aspect), maxHeight)
  False -> (currentWidth, currentHeight)
  where
    isLandscape = currentWidth > currentHeight
    aspect      = ((fromIntegral currentWidth) / (fromIntegral currentHeight))
    resize      = case isLandscape of
      True  -> currentWidth > maxWidth
      False -> currentHeight > maxHeight

-- Default processing of an image
-- Scales original image and rewrites
-- it at destination
-- creates a thumbnail for image
-- returns path to thumbnail
processImage :: T.Text -> T.Text -> Int -> Int -> IO ()
processImage path storePath maxWidth maxHeight = do
  withMagickWandGenesis $ do
    (_, w) <- magickWand
    readImage w path
    width  <- getImageWidth w
    height <- getImageHeight w
    let scaledSizes = getScaledSizes width height maxWidth maxHeight
    resizeImage w (fst scaledSizes) (snd scaledSizes) mirchellFilter 0.6
    setImageCompressionQuality w 100
    writeImages w storePath True
