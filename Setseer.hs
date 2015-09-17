
module Setseer where

import Codec.Picture
import Data.Complex
import Data.Word

import MandelbrotSet

main :: IO ()
main = do
    let w = 768
    let h = 768
    let params = makeMandelbrotParams (w, h)
    let pixel = (mandelbrotMuPixel 0.7 1.0)
    let renderer = (mandelbrotPixelRenderer params pixel)
    
    putStrLn "generateImage ..."
    writePng "test.png" $ generateImage renderer w h
    putStrLn "... done."
