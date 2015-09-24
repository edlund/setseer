{-|
Module      : Main
Description : Main
Copyright   : Erik Edlund
License     : GPL-3
Maintainer  : erik.edlund@32767.se
Stability   : experimental
Portability : POSIX
-}

module Main where

import Codec.Picture
import Data.Complex
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.IO

import Cli
import Setseer.Color
import Setseer.Glue
import Setseer.Pixel

makeSetParams
  :: Options
  -> SetParams
makeSetParams opts
    = SetParams
        (optEscapeIter opts)
        reax
        imax
        ((max' reax - min' reax) / frI w)
        ((max' imax - min' imax) / frI h)
        (optCX opts)
        (optCY opts)
        (generateEscapeColors
          (optStretchR opts)
          (optStretchG opts)
          (optStretchB opts))
  where
    w :: Int
    w = optWidth opts
    h :: Int
    h = optHeight opts
    reax :: Axis
    reax = Axis
      (optReMin opts)
      (optReMax opts)
    imax :: Axis
    imax = Axis
      (optImMin opts)
      (optImMax opts)

requireCreator
  :: (Options, [String])
  -> (SetParams -> (Int -> Int -> PixelRGB8))
requireCreator (_, (modCreator:[]))
    = case lookup modCreator modCreators of
        Nothing -> error $ "could not find module: " ++ modCreator
        Just cr -> cr
requireCreator (_, [])
    = error $ "missing module"
requireCreator (_, unknowns)
    = error $ "confused by: " ++ show unknowns

main
  :: IO ()
main = do
    args <- getArgs
    ropts <- parseArgs args
    
    let opts = fst ropts
    
    let width = optWidth opts
    let height = optHeight opts
    let path = optOutput opts
    
    let creator = requireCreator ropts
    let params = makeSetParams opts
    let renderer = creator params
    
    putOptions opts
    
    writePng path $ generateImage renderer width height
    
    putStrLn $ "result written to " ++ path

