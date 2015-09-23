{-|
Module      : JuliaSet
Description : Glue for rendering the julia set
Copyright   : Erik Edlund
License     : GPL-3
Maintainer  : erik.edlund@32767.se
Stability   : experimental
Portability : POSIX
-}

module JuliaSet where

import Codec.Picture
import Data.Complex

import Cli
import Color
import Glue
import Pixel

juliaCC
  :: SetParams
  -> Int
  -> Int
  -> Complex''
juliaCC params _ _
    = (cx params, cy params)

juliaCZ :: (SetParams -> Int -> Int -> Complex'')
juliaCZ = coordToComplex''

julia
  :: [ArgPair]
  -> SetParams
  -> (Int -> Int -> PixelRGB8)
julia args params
    = (pixelRenderer
        params
        escape_ZZ_plus_C
        juliaCC
        juliaCZ
        escapeColorPixel)

