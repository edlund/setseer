
module Glue where

import Codec.Picture
import Data.Complex
import Data.Fixed
import Data.Word
import Test.QuickCheck

data Axis = Axis 
 { min' :: Double
 , max' :: Double
 }
 deriving (Eq, Show)

data PixelHSVd = PixelHSVd
 Double -- h
 Double -- s
 Double -- v
 deriving (Eq, Ord, Show)

type Complex' = Complex Double

frI :: ((Integral a, Num b) => a -> b)
frI = fromIntegral

convertHSVdToPixelRGB8
  :: Double
  -> Double
  -> Double
  -> PixelRGB8
convertHSVdToPixelRGB8 h s v
    | i == 0
    = makePixel v t p
    | i == 1
    = makePixel q v p
    | i == 2
    = makePixel p v t
    | i == 3
    = makePixel p q v
    | i == 4
    = makePixel t p v
    | i == 5
    = makePixel v p q
  where
    makePixel
      :: Double
      -> Double
      -> Double
      -> PixelRGB8
    makePixel r g b = PixelRGB8
        (round (r * 255.0))
        (round (g * 255.0))
        (round (b * 255.0))
    hh :: Double
    hh = if h >= 360.0 then 0.0 else (h / 60.0)
    i :: Int
    i = floor hh
    f :: Double
    f = hh - frI i
    p :: Double
    p = v * (1.0 - s)
    q :: Double
    q = v * (1.0 - s * f)
    t :: Double
    t = v * (1.0 - s * (1.0 - f))

convertRGB8ToPixelHSVd
  :: Word8
  -> Word8
  -> Word8
  -> PixelHSVd
convertRGB8ToPixelHSVd r g b
    = PixelHSVd (if h < 0 then h + 360.0 else h) s v
  where
    makeH
      :: Double
      -> Double
      -> Double
      -> Double
      -> Double
      -> Double
    makeH r' g' b' xM dX
        | dX == 0
        = 0
        | xM == r'
        = ((g' - b') / dX) `mod'` 6
        | xM == g'
        = ((b' - r') / dX) + 2.0
        | otherwise
        = ((r' - g') / dX) + 4.0
    r' :: Double
    r' = frI r / 255.0
    g' :: Double
    g' = frI g / 255.0
    b' :: Double
    b' = frI b / 255.0
    xm :: Double
    xm = foldl (min) 1 [r', g', b']
    xM :: Double
    xM = foldl (max) 0 [r', g', b']
    dX :: Double
    dX = xM - xm
    h :: Double
    h = (makeH r' g' b' xM dX) * 60
    s :: Double
    s = if xM == 0 then 0 else dX / xM
    v :: Double
    v = xM

prop_RGB2HSV_HSV2RGB :: Word8 -> Word8 -> Word8 -> Bool
prop_RGB2HSV_HSV2RGB r g b
    = (goBack (convertRGB8ToPixelHSVd r g b)) == (PixelRGB8 r g b)
  where
    goBack
      :: PixelHSVd
      -> PixelRGB8
    goBack (PixelHSVd h s v)
        = convertHSVdToPixelRGB8 h s v
