{-# LANGUAGE MultiWayIf #-}
module Image.Outline where

import Graph
import Image.PixelDistance
import MarkovRandomField
import Codec.Picture
import Control.Monad
import qualified Data.HashSet as S
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Tuple

import Debug.Trace

type Coordinate = (Int, Int)

coordinatesToPicture :: Int -> Int -> S.HashSet Coordinate -> Image Pixel8
coordinatesToPicture width height inside = generateImage (curry $! toPixel . isInside) width height
  where toPixel True = maxBound
        toPixel False = 0
        isInside = flip S.member inside

makeMRF :: PixelDistance a => Int -> S.HashSet Coordinate -> S.HashSet Coordinate -> Image a -> MarkovRandomField Int
makeMRF lambda inside outside image@(Image width height _) =
  MarkovRandomField { numVertices = width * height
                    , unaryParams = IM.fromList unis
                    , binaryParams = M.fromList bins
                    , constParam = 0
                    }
  where inf = maxBound `div` 2 :: Int
        xRange = [0 .. width - 1]
        yRange = [0 .. height - 1]
        unis = do
          x <- xRange
          y <- yRange
          let param = if | (x, y) `S.member` inside -> UniParam lambda 0
                         | (x, y) `S.member` outside -> UniParam 0 lambda
                         | otherwise -> UniParam 0 0
          return (pixelBaseIndex image x y, param)
        bins = do
          x <- xRange
          y <- yRange
          guard $ 0 < x && x < width - 1
          guard $ 0 < y && y < height - 1
          dx <- [-1, 1]
          dy <- [-1, 1]
          let (x', y') = (x + dx, y + dy)
              [v, v'] = map (uncurry $ pixelBaseIndex image) [(x, y), (x', y')]
              dist = distance (pixelAt image x y) (pixelAt image x' y')
          return ((v, v'), BinParam 0 dist dist 0)

getOutline :: PixelDistance a => Int -> [Coordinate] -> [Coordinate] -> Image a -> Image Pixel8
getOutline lambda inside outside image@(Image width height _) =
  coordinatesToPicture width height $! S.map vertex2Coordinate $! inVertices
  where inf = maxBound `div` 2
        mrf = makeMRF lambda (S.fromList inside) (S.fromList outside) image
        vertex2Coordinate (Vertex v) = swap $! divMod v width
        (_, inVertices) = minimizeEnergy mrf inf
