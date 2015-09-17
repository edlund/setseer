
module Setseer where

import Codec.Picture
import Data.Complex
import Data.List
import System.IO
import System.Environment

import JuliaSet
import MandelbrotSet

dispatch :: [(String, ([String] -> (Int -> Int -> PixelRGB8)))]
dispatch =
 [ ("mandelbrot", mandelbrot)
 ]

main :: IO ()
main = do
    progname <- getProgName
    cmdline <- getArgs
    if length cmdline > 0
    then do
      let (mod:args) = cmdline
      let (Just creator) = lookup mod dispatch
      let renderer = creator args
      
      let w = 256
      let h = 256
      
      writePng "test.png" $ generateImage renderer w h
      putStrLn "."
    else do
      putStrLn (progname ++ " mod [--modargs]")

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
