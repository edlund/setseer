
module Setseer where

import Codec.Picture
import Data.Complex
import Data.List
import System.IO
import System.Environment

import Cli

import JuliaSet
import MandelbrotSet

mods :: [(String, ([ArgPair] -> (Int, Int) -> (Int -> Int -> PixelRGB8)))]
mods =
 [ ("mandelbrot", mandelbrot)
 , ("julia", julia)
 ]

defaultArgs :: [ArgPair]
defaultArgs =
 [ ("width", "1280")
 , ("height", "1024")
 , ("path", "setseer.png")
 , ("mandelbrot-re-min", "-2.0")
 , ("mandelbrot-re-max", "1.0")
 , ("mandelbrot-im-min", "-1.5")
 , ("mandelbrot-im-max", "1.5")
 , ("mandelbrot-escapeiter", "512")
 , ("mandelbrot-escaperadius", "2")
 , ("mandelbrot-color-s", "0.7")
 , ("mandelbrot-color-v", "1.0")
 ]

main :: IO ()
main = do
    progname <- getProgName
    cmdline <- getArgs
    if length cmdline > 0
    then do
      let (mod:rargs) = cmdline
      let (Just creator) = lookup mod mods
      
      let args = updateArgs (parseArgs rargs) defaultArgs
      
      let w = read (findArgValue "width" args) :: Int
      let h = read (findArgValue "height" args) :: Int
      
      let renderer = creator args (w, h)
      
      putStr "starting generation..."
      writePng (findArgValue "path" args) $ generateImage renderer w h
      putStrLn " done!"
    else do
      putStrLn (progname ++ " mod [--mod-args] [--main-args]")

{-
let w = 256
let h = 256
let params = makeMandelbrotParams (w, h)
let pixel = (mandelbrotMuPixel 0.7 1.0)
let renderer = (mandelbrotPixelRenderer params pixel)

putStrLn "generateImage ..."
writePng "test.png" $ generateImage renderer w h
putStrLn "... done."
-}
