{-|
Module      : Setseer.JuliaSet
Description : Glue for rendering the julia set
Copyright   : Erik Edlund
License     : GPL-3
Maintainer  : erik.edlund@32767.se
Stability   : experimental
Portability : POSIX
-}

module Setseer.JuliaSet where

import Codec.Picture
import Data.Complex

import Setseer.Color
import Setseer.Glue
import Setseer.Pixel

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
  :: SetParams
  -> (Int -> Int -> PixelRGB8)
julia params
    = (pixelRenderer
        params
        escape_ZZ_plus_C
        juliaCC
        juliaCZ
        escapeColorPixel)

