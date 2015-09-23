{-|
Module      : MandelbrotSet
Description : Glue for rendering the mandelbrot set
Copyright   : Erik Edlund
License     : GPL-3
Maintainer  : erik.edlund@32767.se
Stability   : experimental
Portability : POSIX
-}

module MandelbrotSet where

import Codec.Picture
import Data.Complex

import Cli
import Color
import Glue
import Pixel

mandelbrotCC :: (SetParams -> Int -> Int -> Complex'')
mandelbrotCC = coordToComplex''

mandelbrotCZ :: (SetParams -> Int -> Int -> Complex'')
mandelbrotCZ = coordToComplex''

mandelbrot
  :: [ArgPair]
  -> SetParams
  -> (Int -> Int -> PixelRGB8)
mandelbrot args params
    = (pixelRenderer
        params
        escape_ZZ_plus_C
        mandelbrotCC
        mandelbrotCZ
        escapeColorPixel)

