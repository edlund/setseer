
module Glue where

import Data.Complex

data Axis = Axis 
 { min' :: Double
 , max' :: Double
 }
 deriving (Eq, Show)

type Complex' = Complex Double

frI :: ((Integral a, Num b) => a -> b)
frI = fromIntegral
