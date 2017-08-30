{-# LANGUAGE TypeSynonymInstances #-}
module Image.PixelDistance where

import Codec.Picture

class Pixel a => PixelDistance a where
  distance :: a -> a -> Int

instance PixelDistance Pixel8 where
  distance p q = abs $! fromIntegral $! p - q

instance PixelDistance Pixel16 where
  distance p q = abs $! fromIntegral $! p - q

instance PixelDistance PixelF where
  distance p q = abs $! round $! 255 * (p - q)
