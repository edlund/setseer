
module Glue where

import Codec.Picture
import Data.Complex

type Complex' = Complex Double
type Escape = (Int, Complex')

data Axis = Axis 
 { min' :: Double
 , max' :: Double
 }
 deriving (Eq, Show)

data SetParams = SetParams
 { escapeIter   :: Int
 , re           :: Axis
 , im           :: Axis
 , reScale      :: Double
 , imScale      :: Double
 , colors       :: [PixelRGB8]
 }
 deriving (Eq, Show)

frI :: ((Integral a, Num b) => a -> b)
frI = fromIntegral

e :: Double
e = exp 1

escapeRadius :: Double
escapeRadius = 2.0
