{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import qualified Data.Text as T
import Graphics.ImageMagick.MagickWand
import Safe (readMay)
import System.Environment (lookupEnv)

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
        mconcat
          ["Failed to read [[", str, "]] for environment variable ", env]

getScaledSizes ::
  Int -> -- current width of image
  Int -> -- current height of image
  Int -> -- desired width of scaling
  Int -> -- desired height of scaling
  (Int, Int) -- (width, height)
getScaledSizes currentWidth currentHeight maxWidth maxHeight =
  if resize
    then
      if isLandscape
        then (maxWidth, floor (fromIntegral maxWidth / aspect))
        else
          if currentWidth == currentHeight
            then (maxWidth, maxWidth)
            else (floor (fromIntegral maxHeight * aspect), maxHeight)
    else (currentWidth, currentHeight)
  where
    isLandscape = currentWidth > currentHeight
    aspect = fromIntegral currentWidth / fromIntegral currentHeight
    resize =
      if isLandscape
        then currentWidth > maxWidth
        else currentHeight > maxHeight

processImage :: T.Text -> T.Text -> Int -> Int -> IO ()
processImage path storePath maxWidth maxHeight = do
  withMagickWandGenesis $ do
    (_, w) <- magickWand
    readImage w path
    width <- getImageWidth w
    height <- getImageHeight w
    let scaledSizes = getScaledSizes width height maxWidth maxHeight
    uncurry (resizeImage w) scaledSizes mirchellFilter 0.6
    setImageCompressionQuality w 80
    writeImages w storePath True
