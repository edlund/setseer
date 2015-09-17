
module Glue where

import Data.Complex

type Complex' = Complex Double

data Axis = Axis 
 { min' :: Double
 , max' :: Double
 }
 deriving (Eq, Show)

frI :: ((Integral a, Num b) => a -> b)
frI = fromIntegral
