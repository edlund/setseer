{-|
Module      : Check
Description : Testing
Copyright   : Erik Edlund
License     : GPL-3
Maintainer  : erik.edlund@32767.se
Stability   : experimental
Portability : POSIX
-}

module Check where

import Test.QuickCheck

import Glue
import Color

import JuliaSet
import MandelbrotSet

check
  :: (Testable prop)
  => prop
  -> String
  -> IO ()
check p name = do
    putStr $ name ++ ": "
    quickCheck p

main :: IO ()
main = do
    check prop_RGB2HSV_HSV2RGB "color:prop_RGB2HSV_HSV2RGB"

