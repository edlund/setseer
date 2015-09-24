{-|
Module      : Cli
Description : A simplistic command line interface
Copyright   : Erik Edlund
License     : GPL-3
Maintainer  : erik.edlund@32767.se
Stability   : experimental
Portability : POSIX
-}

module Cli where

import Codec.Picture
import Data.Maybe
import System.Console.GetOpt
import Text.Printf

import Setseer.Glue

import Setseer.JuliaSet
import Setseer.MandelbrotSet

data Options = Options
 { optOutput      :: FilePath
 , optWriter      :: String
 , optWidth       :: Int
 , optHeight      :: Int
 , optStretchR    :: Double
 , optStretchG    :: Double
 , optStretchB    :: Double
 , optReMin       :: Double
 , optReMax       :: Double
 , optImMin       :: Double
 , optImMax       :: Double
 , optCX          :: Double
 , optCY          :: Double
 , optEscapeIter  :: Int
 }
 deriving
 ( Eq
 , Show
 )

defaultOptions :: Options
defaultOptions
 = Options
 { optOutput      = "setseer.png"
 , optWriter      = "png"
 , optWidth       = 1280
 , optHeight      = 1024
 , optStretchR    = 1.0
 , optStretchG    = 1.0
 , optStretchB    = 1.0
 , optReMin       = -2.0
 , optReMax       = 2.0
 , optImMin       = -1.5
 , optImMax       = 1.5
 , optCX          = -0.75
 , optCY          = -0.20
 , optEscapeIter  = 64
 }

modCreators :: [(String, (SetParams -> (Int -> Int -> PixelRGB8)))]
modCreators =
 [ ("mandelbrot", mandelbrot)
 , ("julia", julia)
 ]

optDescriptions :: [OptDescr (Options -> Options)]
optDescriptions =
 [ Option ['w'] ["width"] (ReqArg ((\ v opts
       -> opts { optWidth = abs $ read v :: Int })) "Int")
     "image width"
 , Option ['h'] ["height"] (ReqArg ((\ v opts
       -> opts { optHeight = abs $ read v :: Int })) "Int")
     "image height"
 , Option ['o'] ["output"] (ReqArg ((\ v opts
       -> opts { optOutput = v })) "Path")
     "image path"
 
 , Option [] ["stretch-r"] (ReqArg ((\ v opts
       -> opts { optStretchR = read v :: Double })) "Double")
     "red stretch"
 , Option [] ["stretch-g"] (ReqArg ((\ v opts
       -> opts { optStretchG = read v :: Double })) "Double")
     "green stretch"
 , Option [] ["stretch-b"] (ReqArg ((\ v opts
       -> opts { optStretchB = read v :: Double })) "Double")
     "blue stretch"
 
 , Option ['r'] ["remin"] (ReqArg ((\ v opts
       -> opts { optReMin = read v :: Double })) "Double")
     "real min"
 , Option ['R'] ["remax"] (ReqArg ((\ v opts
       -> opts { optReMax = read v :: Double })) "Double")
     "real max"
 , Option ['i'] ["immin"] (ReqArg ((\ v opts
       -> opts { optImMin = read v :: Double })) "Double")
     "imag min"
 , Option ['I'] ["immax"] (ReqArg ((\ v opts
       -> opts { optImMax = read v :: Double })) "Double")
     "imag max"
 
 , Option ['X'] ["cx"] (ReqArg ((\ v opts
       -> opts { optCX = read v :: Double })) "Double")
     "cX constant"
 , Option ['Y'] ["cy"] (ReqArg ((\ v opts
       -> opts { optCY = read v :: Double })) "Double")
     "cY constant"
 
 , Option ['n'] ["iter"] (ReqArg ((\ v opts
       -> opts { optEscapeIter = read v :: Int })) "Int")
     "escape iteration"
 ]

parseArgs
  :: [String]
  -> IO (Options, [String])
parseArgs argv
    | "--help" `elem` argv
    = ioError $ userError $ usageInfo header optDescriptions
    | otherwise
    = case getOpt Permute optDescriptions argv of
        (opts, xs, []) -> return (foldl (flip id) defaultOptions opts, xs)
        (_, _, errors) -> ioError (userError (concat errors
            ++ usageInfo header optDescriptions))
  where
    header :: String
    header = "Usage: setseer mandelbrot|julia [--options]"

putOptions
  :: Options
  -> IO ()
putOptions opts
    = printf fmt
      (show (optOutput opts))
      (show (optWriter opts))
      (optWidth opts)
      (optHeight opts)
      (optStretchR opts)
      (optStretchG opts)
      (optStretchB opts)
      (optReMin opts)
      (optReMax opts)
      (optImMin opts)
      (optImMax opts)
      (optCX opts)
      (optCY opts)
      (optEscapeIter opts)
  where
    fmt :: String
    fmt = unlines
      [ "Conf"
      , "-"
      , "Output:    %s"
      , "Writer:    %s"
      , "Width:     %dpx"
      , "Height:    %dpx"
      , "StretchR:  %f"
      , "StretchG:  %f"
      , "StretchB:  %f"
      , "Re:        %f <= x <= %f"
      , "Im:        %f <= y <= %f"
      , "cX:        %f"
      , "cY:        %f"
      , "EscIter:   %d"
      , "-"
      ]

