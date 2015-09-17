
module MandelbrotSet where

import Codec.Picture
import Data.Complex

import Cli
import Color
import Glue

data MandelbrotParams = MandelbrotParams
 { escapeRadius :: Double
 , escapeIter   :: Int
 , re           :: Axis
 , im           :: Axis
 , reScale      :: Double
 , imScale      :: Double
 }
 deriving (Eq, Show)

type Escape = (Int, Complex')

mandelbrotMu
  :: MandelbrotParams
  -> Escape
  -> Double
mandelbrotMu params escs
    = (frI n + 1.0 - log (log a) / log 2)
  where
    n :: Int
    n = fst escs
    z :: Complex'
    z = snd escs
    a :: Double
    a = realPart (abs z)

mandelbrotSimplePixel
  :: Double
  -> Double
  -> MandelbrotParams
  -> Escape
  -> PixelRGB8
mandelbrotSimplePixel s v params escs
    | fst escs < escapeIter params
    = convertHSVdToPixelRGB8 h s v
    | otherwise
    = PixelRGB8 0 0 0
  where
    m :: Double
    m = (mandelbrotMu params escs) / frI (escapeIter params)
    h :: Double
    h = 1.0 + 10.0 * m

mandelbrotPixelRenderer
  :: MandelbrotParams
  -> (MandelbrotParams -> Escape -> PixelRGB8)
  -> Int
  -> Int
  -> PixelRGB8
mandelbrotPixelRenderer params pxl x y
    = pxl params escs
  where
    mandelbrotEsc
      :: MandelbrotParams
      -> Int
      -> Complex'
      -> Complex'
      -> Escape
    mandelbrotEsc params i c z
        | i < escI && realPart (abs z) < escR
        = mandelbrotEsc params (i + 1) (c) (z ^ 2 + c)
        | otherwise
        = (i, z)
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
    escs :: Escape
    escs = mandelbrotEsc params 0 c0 c0

makeMandelbrotParams
  :: [ArgPair]
  -> (Int, Int)
  -> MandelbrotParams
makeMandelbrotParams args dims
    = MandelbrotParams
        (read (findArgValue "mandelbrot-escaperadius" args) :: Double)
        (read (findArgValue "mandelbrot-escapeiter" args) :: Int)
        reax
        imax
        ((max' reax - min' reax) / frI w)
        ((max' imax - min' imax) / frI h)
  where
    w :: Int
    w = fst dims
    h :: Int
    h = snd dims
    reax :: Axis
    reax = Axis
      (read (findArgValue "mandelbrot-re-min" args) :: Double)
      (read (findArgValue "mandelbrot-re-max" args) :: Double)
    imax :: Axis
    imax = Axis
      (read (findArgValue "mandelbrot-im-min" args) :: Double)
      (read (findArgValue "mandelbrot-im-max" args) :: Double)

mandelbrot
  :: [ArgPair]
  -> (Int, Int)
  -> (Int -> Int -> PixelRGB8)
mandelbrot args dims
    = (mandelbrotPixelRenderer params
        (mandelbrotSimplePixel
          ((read (findArgValue "mandelbrot-color-s" args) :: Double))
          ((read (findArgValue "mandelbrot-color-v" args) :: Double))))
  where
    params :: MandelbrotParams
    params = makeMandelbrotParams args dims
