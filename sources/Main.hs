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
import Control.Exception
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
    w = optWidth opts
    h = optHeight opts
    reax = Axis
      (optReMin opts)
      (optReMax opts)
    imax = Axis
      (optImMin opts)
      (optImMax opts)

requireCreator
  :: [String]
  -> (SetParams -> (Int -> Int -> PixelRGB8))
requireCreator (modCreator:[])
    = case lookup modCreator modCreators of
        Nothing -> error $ "Could not find module: " ++ modCreator
        Just cr -> cr
requireCreator []
    = error $ "Missing module"
requireCreator unknowns
    = error $ "Confused by: " ++ show unknowns

go
  :: Bool
  -> OptionsPair
  -> IO ()
go interactive (opts, args) = do
    let width = optWidth opts
    let height = optHeight opts
    let path = optOutput opts
    let creator = requireCreator args
    let params = makeSetParams opts
    let renderer = creator params
    if interactive
      then do putStrLn "Generating and writing"
      else do putOptions opts
    writePng path $ generateImage renderer width height
    putStrLn $ "Result written to " ++ show path

step
  :: OptionsPair
  -> IO ()
step op = do
    putOptions $ fst op
    putStr "? "
    hFlush stdout
    ln <- getLine
    peak (words ln) op
  where
    -- Handle a parseArgs error.
    again op err = do
        putStrLn err
        step op
    -- Handle a blank line.
    peak [] op = do
        step op
    -- Adjust the options, try to parse ws.
    peak (":a":ws) (opts, args) = do
        ropts <- try (parseArgs ws opts) :: IO (Either
                SomeException
                OptionsPair
            )
        case ropts of
            Left ex  -> again (opts, args) (show ex)
            Right ro -> step (fst ro, args)
    -- Write the result to a file.
    peak (":w":[]) op = do
        go True op
        step op
    -- Quit interactive mode.
    peak (":q":[]) _ = do
        putStrLn "Goodbye, have a nice day"
    -- Handle an unknown command.
    peak (x:_) op = do
        putStrLn $ "Unknown command: " ++ x
        step op

main
  :: IO ()
main = do
    args <- getArgs
    ropts <- parseArgs args defaultOptions
    if "-$" `elem` args || "--interactive" `elem` args
      then do step ropts
      else do go False ropts

