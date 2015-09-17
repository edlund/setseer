
module Setseer where

import Codec.Picture
import Data.Complex

import Glue

data MandelbrotParams = MandelbrotParams
 { escapeRadius :: Double
 , escapeIter   :: Int
 , re           :: Axis
 , im           :: Axis
 , reScale      :: Double
 , imScale      :: Double
 , baseRGB      :: PixelRGB8
 }
 deriving (Eq, Show)

makeMandelbrotParams
  :: (Int, Int)
  -> MandelbrotParams
makeMandelbrotParams dims = MandelbrotParams
    2.0
    128
    reax
    imax
    ((max' reax - min' reax) / frI w)
    ((max' imax - min' imax) / frI h)
    (PixelRGB8 63 127 255)
  where
    w :: Int
    w = fst dims
    h :: Int
    h = snd dims
    reax :: Axis
    reax = Axis (-2.0) 0.5
    imax :: Axis
    imax = Axis (-1.0) 1.0

mandelbrotSimplePixel
  :: MandelbrotParams
  -> Int
  -> PixelRGB8
  -> PixelRGB8
mandelbrotSimplePixel params escN (PixelRGB8 r g b) = PixelRGB8
    (truncate (frI r * esc))
    (truncate (frI g * esc))
    (truncate (frI b * esc))
  where
    esc :: Double
    esc = frI escN / frI (escapeIter params)

mandelbrotPixelRenderer
  :: MandelbrotParams
  -> (MandelbrotParams -> Int -> PixelRGB8 -> PixelRGB8)
  -> Int
  -> Int
  -> PixelRGB8
mandelbrotPixelRenderer params pxl x y = pxl params escN (baseRGB params)
  where
    mandelbrotEsc
      :: MandelbrotParams
      -> Int
      -> Complex'
      -> Complex'
      -> Int
    mandelbrotEsc params i c z | i < escI && realPart (abs z) < escR
                               = mandelbrotEsc params (i + 1) (c) (z ^ 2 + c)
                               | otherwise
                               = escI - i
    cX :: Double
    cX = frI x * reScale params + min' (re params)
    cY :: Double
    cY = frI y * imScale params + min' (im params)
    c0 :: Complex'
    c0 = (cX :+ cY)
    escR :: Double
    escR = escapeRadius params
    escI :: Int
    escI = escapeIter params
    escN :: Int
    escN = mandelbrotEsc params 0 c0 c0

main :: IO ()
main = do
    let w = 128
    let h = 128
    let params = makeMandelbrotParams (w, h)
    let renderer = (mandelbrotPixelRenderer params mandelbrotSimplePixel)
    
    putStrLn "generateImage ..."
    writePng "test.png" $ generateImage renderer w h
    putStrLn "... done."
