module Main where

import Codec.Picture
import Image.PixelDistance
import Image.Outline
import Util
import System.Environment

saveConvertedImage :: PixelDistance a => Int -> FilePath -> [Coordinate] -> [Coordinate] -> Image a -> IO ()
saveConvertedImage lambda path inside outside img = writePng path $ getOutline lambda inside outside img

main :: IO ()
main = do
  args <- getArgs
  case args of
    [lambda', imgPath, inPoss', outPoss', outPath] -> do
      lambda <- readIO lambda'
      (Right (ImageY8 inPoss)) <- readImage inPoss'
      (Right (ImageY8 outPoss)) <- readImage outPoss'
      image' <- readImage imgPath
      let inCoords = imageToCoordinates inPoss
          outCoords = imageToCoordinates outPoss
      case image' of
        Left err -> putStrLn err
        Right (ImageY8 image) -> saveConvertedImage lambda outPath inCoords outCoords image
        Right (ImageY16 image) -> saveConvertedImage lambda outPath inCoords outCoords image
        Right (ImageYF image) -> saveConvertedImage lambda outPath inCoords outCoords image
        _ -> fail "Unsupported image type"
    _ -> fail "please supply 6 arguments"
