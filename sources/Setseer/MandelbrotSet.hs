{-|
Module      : Setseer.MandelbrotSet
Description : Glue for rendering the mandelbrot set
Copyright   : Erik Edlund
License     : GPL-3
Maintainer  : erik.edlund@32767.se
Stability   : experimental
Portability : POSIX
-}

module Setseer.MandelbrotSet where

import Codec.Picture
import Data.Complex

import Setseer.Color
import Setseer.Glue
import Setseer.Pixel

mandelbrotCC :: (SetParams -> Int -> Int -> Complex'')
mandelbrotCC = coordToComplex''

mandelbrotCZ :: (SetParams -> Int -> Int -> Complex'')
mandelbrotCZ = coordToComplex''

mandelbrot
  :: SetParams
  -> (Int -> Int -> PixelRGB8)
mandelbrot params
    = (pixelRenderer
        params
        escape_ZZ_plus_C
        mandelbrotCC
        mandelbrotCZ
        escapeColorPixel)

