
module Setseer where

import Codec.Picture
import Data.Complex
import Data.List
import System.IO
import System.Environment
import Text.Regex.Posix

import Cli

import JuliaSet
import MandelbrotSet

dispatch :: [(String, ([ArgPair] -> (Int -> Int -> PixelRGB8)))]
dispatch =
 [ ("mandelbrot", mandelbrot)
 , ("julia", julia)
 ]

main :: IO ()
main = do
    progname <- getProgName
    cmdline <- getArgs
    if length cmdline > 0
    then do
      let (mod:rargs) = cmdline
      let (Just creator) = lookup mod dispatch
      
      let args = parseArgs rargs
      let renderer = creator args
      
      let w = 256
      let h = 256
      
      writePng "setseer.png" $ generateImage renderer w h
      putStrLn "."
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
