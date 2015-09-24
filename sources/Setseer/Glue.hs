{-|
Module      : Setseer.Glue
Description : A few things to hold setseer together
Copyright   : Erik Edlund
License     : GPL-3
Maintainer  : erik.edlund@32767.se
Stability   : experimental
Portability : POSIX
-}

module Setseer.Glue where

import Codec.Picture
import Data.Complex
import Data.Vector

type Complex' = Complex Double
type Complex'' = (Double, Double)
type Escape = (Int, Complex')

data Axis = Axis 
 { min' :: Double
 , max' :: Double
 }
 deriving
 ( Eq
 , Show
 )

data SetParams = SetParams
 { escapeIter   :: Int
 , re           :: Axis
 , im           :: Axis
 , reScale      :: Double
 , imScale      :: Double
 , cx           :: Double
 , cy           :: Double
 , colors       :: Vector PixelRGB8
 }
 deriving
 ( Eq
 , Show
 )

frI :: ((Integral a, Num b) => a -> b)
frI = fromIntegral

e :: Double
e = exp 1

ee :: Double
ee = e * e

logFor2:: Double
logFor2 = log 2

escapeRadius :: Double
escapeRadius = 2.0

escapeLimit :: Double
escapeLimit = escapeRadius * escapeRadius

coordToComplex''
  :: SetParams
  -> Int
  -> Int
  -> Complex''
coordToComplex'' params x y
    = (cX, cY)
  where
    cX :: Double
    cX = frI x * reScale params + min' (re params)
    cY :: Double
    cY = frI y * imScale params + min' (im params)

