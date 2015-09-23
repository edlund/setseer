{-|
Module      : Main
Description : Main
Copyright   : Erik Edlund
License     : GPL-3
Maintainer  : erik.edlund@32767.se
Stability   : experimental
Portability : POSIX
-}

-- To profile:
-- ghc -prof -fprof-auto -rtsopts Main.hs
-- ghc -prof -fprof-auto -rtsopts -fexcess-precision -fforce-recomp Main.hs

module Main where

import Codec.Picture
import Data.Complex
import Data.List
import System.IO
import System.Environment

import Cli
import Color
import Glue
import Pixel

import JuliaSet
import MandelbrotSet

mods :: [(String, ([ArgPair] -> SetParams -> (Int -> Int -> PixelRGB8)))]
mods =
 [ ("mandelbrot", mandelbrot)
 , ("julia", julia)
 ]

defaultArgs :: [ArgPair]
defaultArgs =
 [ ("width", "1280")
 , ("height", "1024")
 , ("path", "setseer.png")
 , ("color-stretch-r", "1.0")
 , ("color-stretch-g", "1.0")
 , ("color-stretch-b", "1.0")
 , ("re-min", "-2.0")
 , ("re-max", "2.0")
 , ("im-min", "-1.5")
 , ("im-max", "1.5")
 , ("escapeiter", "64")
 , ("cx", "-0.75")
 , ("cy", "-0.20")
 ]

makeSetParams
  :: [ArgPair]
  -> (Int, Int)
  -> SetParams
makeSetParams args (w, h)
    = SetParams
        (read (findArgValue "escapeiter" args) :: Int)
        reax
        imax
        ((max' reax - min' reax) / frI w)
        ((max' imax - min' imax) / frI h)
        (read (findArgValue "cx" args) :: Double)
        (read (findArgValue "cy" args) :: Double)
        (generateEscapeColors
          (read (findArgValue "color-stretch-r" args) :: Double)
          (read (findArgValue "color-stretch-g" args) :: Double)
          (read (findArgValue "color-stretch-b" args) :: Double))
  where
    reax :: Axis
    reax = Axis
      (read (findArgValue "re-min" args) :: Double)
      (read (findArgValue "re-max" args) :: Double)
    imax :: Axis
    imax = Axis
      (read (findArgValue "im-min" args) :: Double)
      (read (findArgValue "im-max" args) :: Double)

main :: IO ()
main = do
    progname <- getProgName
    cmdline <- getArgs
    if length cmdline > 0
    then do
      let (mod:rargs) = cmdline
      let (Just creator) = lookup mod mods
      
      let args = updateArgs (parseArgs rargs) defaultArgs
      
      let path = findArgValue "path" args
      let width = read (findArgValue "width" args) :: Int
      let height = read (findArgValue "height" args) :: Int
      let dims = (width, height)
      
      let params = makeSetParams args dims
      let renderer = creator args params
      
      putStrLn "placing pixels..."
      writePng path $ generateImage renderer width height
      
      putStrLn $ "result written to " ++ path
    else do
      putStrLn $ progname ++ " mod [--mod-args=xs] [--main-args=ys]"

