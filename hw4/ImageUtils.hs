{-# OPTIONS_GHC -fwarn-tabs -Wno-incomplete-uni-patterns #-}

module ImageUtils where

import Codec.Picture ( generateImage, Image, Pixel, Pixel8 )
import System.Random

--
-- Support for streams of random numbers
--

-- | A stream of random numbers, each in [-10, 1.0]
type RandomDoubles = [Double]

splitRandomDoubles :: RandomDoubles -> (RandomDoubles, RandomDoubles)
splitRandomDoubles ~(x:y:zs) = (x:r, y:s)
  where
    (r, s) = splitRandomDoubles zs

mkRandomDoubles :: RandomGen g => g -> RandomDoubles
mkRandomDoubles g = x : mkRandomDoubles g'
  where
    (x, g') = uniformR (0.0, 1.0) g

--
-- Low-level utilities
--

-- | Convert the position `pos` in [0, 2*n] to a value in [-1.0, 1.0]
toPos :: Int -> Int -> Double
toPos n pos = fromIntegral (pos - n) / fromIntegral n

-- | Converts alpha in [-1.0, 1.0] to a value in [0, 255]
toPixel8 :: Double -> Pixel8
toPixel8 alpha = round $ (alpha + 1.0) * 255.0/2.0

mkImage :: Pixel a => Int -> (Int -> Int -> a) -> Image a
mkImage n f = generateImage g dim dim
  where
    dim = 2*n

    g x y = f x (dim - y)
