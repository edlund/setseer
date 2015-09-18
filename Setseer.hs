
module Setseer where

import Codec.Picture
import Data.Complex
import Data.List
import System.IO
import System.Environment

import Cli
import Color
import Glue

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
 , ("re-max", "1.0")
 , ("im-min", "-1.5")
 , ("im-max", "1.5")
 , ("escapeiter", "512")
 ]

makeSetParams
  :: [ArgPair]
  -> (Int, Int)
  -> SetParams
makeSetParams args dims
    = SetParams
        (read (findArgValue "escapeiter" args) :: Int)
        reax
        imax
        ((max' reax - min' reax) / frI w)
        ((max' imax - min' imax) / frI h)
        (generateEscapeColors
          0
          (read (findArgValue "color-stretch-r" args) :: Double)
          (read (findArgValue "color-stretch-g" args) :: Double)
          (read (findArgValue "color-stretch-b" args) :: Double))
  where
    w :: Int
    w = fst dims
    h :: Int
    h = snd dims
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
      
      let width = read (findArgValue "width" args) :: Int
      let height = read (findArgValue "height" args) :: Int
      
      let params = makeSetParams args (width, height)
      let renderer = creator args params
      
      putStrLn "starting generation..."
      writePng (findArgValue "path" args)
        $ generateImage renderer width height
      putStrLn "... done!"
    else do
      putStrLn (progname ++ " mod [--mod-args] [--main-args]")
