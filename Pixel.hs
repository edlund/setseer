{-|
Module      : Pixel
Description : Generate pixels
Copyright   : Erik Edlund
License     : GPL-3
Maintainer  : erik.edlund@32767.se
Stability   : experimental
Portability : POSIX
-}

module Pixel where

import Codec.Picture
import Data.Complex
import Data.Vector
import Data.Word

import Color
import Glue

escapeColorPixel
  :: SetParams
  -> Escape
  -> PixelRGB8
escapeColorPixel params escs
    | n < 256
    = colors params ! n
    | otherwise
    = PixelRGB8 0 0 0
  where
    cX :: Double
    cX = realPart $ snd escs
    cY :: Double
    cY = imagPart $ snd escs
    sqr :: Double
    sqr = (cX * cX) + (cY * cY)
    adj :: Double
    adj = if sqr > ee
          then log ((log sqr) / 2.0) / logFor2
          else 0.0
    a :: Int
    a = (fst escs - truncate adj) * 255
    n :: Int
    n = abs $ truncate $ (frI a) / (frI (escapeIter params))

rainbowPixel
  :: SetParams
  -> Escape
  -> PixelRGB8
rainbowPixel params escs
    | fst escs < escapeIter params
    = convertHSVdToPixelRGB8 h s v
    | otherwise
    = PixelRGB8 0 0 0
  where
    s :: Double
    s = 0.7
    v :: Double
    v = 1.0
    n :: Int
    n = fst escs
    z :: Complex'
    z = snd escs
    a :: Double
    a = realPart $ abs z
    m :: Double
    m = frI n + 1.0 - log (log a) / logFor2
    h :: Double
    h = frI (escapeIter params - fst escs) / m

-- Escape for `z^(2) + c`; handles Mandelbrot and Julia sets
-- when given appropriate values for `c` and `z`.
escape_ZZ_plus_C
  :: SetParams
  -> Int
  -> Complex''
  -> Complex''
  -> Escape
escape_ZZ_plus_C params i (cX, cY) (zX, zY)
    | i < escapeIter params && m < escapeLimit
    = escape_ZZ_plus_C params (i + 1) (cX, cY) (zX', zY')
    | otherwise
    = (i, (zX :+ zY))
  where
    m :: Double
    m = (zX * zX) + (zY * zY)
    zX' :: Double
    zX' = zX * zX - zY * zY + cX
    zY' :: Double
    zY' = 2.0 * zX * zY + cY

-- Curry to taste.
pixelRenderer
  :: SetParams
  -> (SetParams -> Int -> Complex'' -> Complex'' -> Escape)
  -> (SetParams -> Int -> Int -> Complex'')
  -> (SetParams -> Int -> Int -> Complex'')
  -> (SetParams -> Escape -> PixelRGB8)
  -> Int
  -> Int
  -> PixelRGB8
pixelRenderer params esc cC cZ pxl x y
    = pxl params escs
  where
    c :: Complex''
    c = cC params x y
    z :: Complex''
    z = cZ params x y
    escs :: Escape
    escs = esc params 0 c z

