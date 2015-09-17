
module Glue where

import Codec.Picture
import Data.Complex

data Axis = Axis 
 { min' :: Double
 , max' :: Double
 }
 deriving (Eq, Show)

type Complex' = Complex Double

frI :: ((Integral a, Num b) => a -> b)
frI = fromIntegral

convertHSVtoPixelRGB8
  :: Double
  -> Double
  -> Double
  -> PixelRGB8
convertHSVtoPixelRGB8 h s v
    | n == 0
    = makePixel v t p
    | n == 1
    = makePixel q v p
    | n == 2
    = makePixel p v t
    | n == 3
    = makePixel p q v
    | n == 4
    = makePixel t p v
    | n == 5
    = makePixel v p q
  where
    makePixel
      :: Double
      -> Double
      -> Double
      -> PixelRGB8
    makePixel r g b = PixelRGB8
        (round (r * 255.0))
        (round (b * 255.0))
        (round (g * 255.0))
    i :: Int
    i = floor (h * 6.0)
    f :: Double
    f = h * 6 - frI i
    p :: Double
    p = v * (1.0 - s)
    q :: Double
    q = v * (1.0 - f * s)
    t :: Double
    t = v * (1.0 - (1.0 - f) * s);
    n :: Int
    n = i `mod` 6
