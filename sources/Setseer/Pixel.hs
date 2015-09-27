{-|
Module      : Setseer.Pixel
Description : Generate pixels
Copyright   : Erik Edlund
License     : GPL-3
Maintainer  : erik.edlund@32767.se
Stability   : experimental
Portability : POSIX
-}

module Setseer.Pixel where

import Codec.Picture
import Data.Complex
import Data.Vector
import Data.Word

import Setseer.Color
import Setseer.Glue

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
    cX = realPart $ snd escs
    cY = imagPart $ snd escs
    sqr = (cX * cX) + (cY * cY)
    adj = if sqr > ee
          then log ((log sqr) / 2.0) / logFor2
          else 0.0
    a = (fst escs - truncate adj) * 255
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
    s = 0.7
    v = 1.0
    n = fst escs
    z = snd escs
    a = realPart $ abs z
    m = frI n + 1.0 - log (log a) / logFor2
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
    m = (zX * zX) + (zY * zY)
    zX' = zX * zX - zY * zY + cX
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
    c = cC params x y
    z = cZ params x y
    escs = esc params 0 c z

