
module MandelbrotSet where

import Codec.Picture
import Data.Complex

import Cli
import Color
import Glue

mandelbrotPixelRenderer
  :: SetParams
  -> (SetParams -> Escape -> PixelRGB8)
  -> Int
  -> Int
  -> PixelRGB8
mandelbrotPixelRenderer params pxl x y
    = pxl params escs
  where
    mandelbrotEsc
      :: SetParams
      -> Int
      -> Complex'
      -> Complex'
      -> Escape
    mandelbrotEsc params i c z
        | i < escI && realPart (abs z) < escapeRadius
        = mandelbrotEsc params (i + 1) (c) (z ^ 2 + c)
        | otherwise
        = (i, z)
    cX :: Double
    cX = frI x * reScale params + min' (re params)
    cY :: Double
    cY = frI y * imScale params + min' (im params)
    c0 :: Complex'
    c0 = (cX :+ cY)
    escI :: Int
    escI = escapeIter params
    escs :: Escape
    escs = mandelbrotEsc params 0 c0 c0

mandelbrot
  :: [ArgPair]
  -> SetParams
  -> (Int -> Int -> PixelRGB8)
mandelbrot args params
    = (mandelbrotPixelRenderer params escapeColorPixel)
