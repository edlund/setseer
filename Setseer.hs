
module Setseer where

import Codec.Picture
import Data.Complex

data Axis = Axis 
 { min' :: Double
 , max' :: Double
 }
 deriving (Eq, Show)

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

type Complex' = Complex Double

frI :: ((Integral a, Num b) => a -> b)
frI = fromIntegral

makeMandelbrotParams :: (Int, Int) -> MandelbrotParams
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

mandelbrotPixelRenderer :: MandelbrotParams -> Int -> Int -> PixelRGB8
mandelbrotPixelRenderer params x y = makePixel esc (baseRGB params)
  where
    mandelbrotEsc :: MandelbrotParams -> Int -> Complex' -> Complex' -> Double
    mandelbrotEsc params i c z | i < escI && realPart (abs z) < escR
                               = mandelbrotEsc params (i + 1) (c) (z ^ 2 + c)
                               | otherwise
                               = frI (escI - i) / frI escI
    makePixel :: Double -> PixelRGB8 -> PixelRGB8
    makePixel e (PixelRGB8 r g b) = PixelRGB8
        (truncate (frI r * e))
        (truncate (frI g * e))
        (truncate (frI b * e))
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
    esc :: Double
    esc = mandelbrotEsc params 0 c0 c0

main :: IO ()
main = do
    let w = 1920
    let h = 1080
    let params = makeMandelbrotParams (w, h)
    let renderer = (mandelbrotPixelRenderer params)
    
    putStrLn "generateImage ..."
    writePng "test.png" $ generateImage renderer w h
    putStrLn "... done."
