{-|
Module      : Color
Description : Conversion and generation
Copyright   : Erik Edlund
License     : GPL-3
Maintainer  : erik.edlund@32767.se
Stability   : experimental
Portability : POSIX
-}

module Color where

import Codec.Picture
import Data.Complex
import Data.Fixed
import Data.List
import Data.Vector
import Data.Word
import Test.QuickCheck

import Glue

data PixelHSVd = PixelHSVd
 !Double -- h
 !Double -- s
 !Double -- v
 deriving (Eq, Ord, Show)

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
    | otherwise
    = error ("convertHSVdToPixelRGB8: invalid i: " Data.List.++ show i)
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
    hh = abs ((if h >= 360.0 then (h `mod'` 360.0) else h) / 60.0)
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
    xm = Data.List.foldl (min) 1 [r', g', b']
    xM :: Double
    xM = Data.List.foldl (max) 0 [r', g', b']
    dX :: Double
    dX = xM - xm
    h :: Double
    h = (makeH r' g' b' xM dX) * 60
    s :: Double
    s = if xM == 0 then 0 else dX / xM
    v :: Double
    v = xM

prop_RGB2HSV_HSV2RGB
  :: Word8
  -> Word8
  -> Word8
  -> Bool
prop_RGB2HSV_HSV2RGB r g b
    = (goBack (convertRGB8ToPixelHSVd r g b)) == (PixelRGB8 r g b)
  where
    goBack
      :: PixelHSVd
      -> PixelRGB8
    goBack (PixelHSVd h s v)
        = convertHSVdToPixelRGB8 h s v

generateEscapeColors
  :: Double
  -> Double
  -> Double
  -> Vector PixelRGB8
generateEscapeColors rStretch gStretch bStretch
    = fromList $ generate 0 rStretch gStretch bStretch
  where
    generate
      :: Int
      -> Double
      -> Double
      -> Double
      -> [PixelRGB8]
    generate n rStretch gStretch bStretch
        | n > 255
        = []
        | otherwise
        = PixelRGB8 r' g' b' : (generate (n + 1)
            rStretch
            gStretch
            bStretch)
      where
        x :: Double
        x = (frI n * 2.0) / 256.0
        r :: Int
        r = truncate (rs * (1.0 + cos ((x - 1.0) * pi)))
        g :: Int
        g = truncate (gs * (1.0 + cos ((x - 1.0) * pi)))
        b :: Int
        b = truncate (bs * (1.0 + sin ((x - 1.0) * pi)))
        r' :: Word8
        r' = frI $ min r 255
        g' :: Word8
        g' = frI $ min g 255
        b' :: Word8
        b' = frI $ min b 255
    rs :: Double
    rs = rStretch * 127.5
    gs :: Double
    gs = gStretch * 127.5
    bs :: Double
    bs = bStretch * 127.5

