module Util where

import Codec.Picture
import Image.Outline

imageToCoordinates :: Image Pixel8 -> [Coordinate]
imageToCoordinates img@(Image width height _) = [ (x, y)
                                                | x <- [0 .. width - 1]
                                                , y <- [0 .. height - 1]
                                                , pixelAt img x y > 0 ]
