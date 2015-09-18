
module Cli where

import Data.Foldable
import Data.List
import Data.Sequence
import Text.Regex.Posix

type ArgPair = (String, String)
type ArgMatch = (String, String, String, [String])

argName
  :: ArgPair
  -> String
argName (n, v)
    = n

argValue
  :: ArgPair
  -> String
argValue (n, v)
    = v

findArgPair
  :: String
  -> [ArgPair]
  -> ArgPair
findArgPair name (arg:args)
    | name == argName arg
    = arg
    | Data.List.null args
    = error ("lookupArgPair: could not find " ++ name)
    | otherwise
    = findArgPair name args

findArgName
  :: String
  -> [ArgPair]
  -> String
findArgName name args
    = argName (findArgPair name args)

findArgValue
  :: String
  -> [ArgPair]
  -> String
findArgValue name args
    = argValue (findArgPair name args)

parseArgs
  :: [String]
  -> [ArgPair]
parseArgs []
    = []
parseArgs (arg:args)
    = transform matches : parseArgs args
  where
    transform
      :: ArgMatch
      -> ArgPair
    transform (left, full, _, matches)
        = (name, value)
      where
        name :: String
        name = if Data.List.null matches then "" else matches !! 0
        value :: String
        value = if Data.List.null matches then left else matches !! 2
    pattern :: String
    pattern = "^--([-a-zA-Z0-9]*)(=([-a-zA-Z0-9 ]+))?$"
    matches :: ArgMatch
    matches = arg =~ pattern :: ArgMatch

updateArgs
  :: [ArgPair]
  -> [ArgPair]
  -> [ArgPair]
updateArgs [] defaults
    = defaults
updateArgs augments defaults
    = toList (walk augments (fromList defaults))
  where
    walk
      :: [ArgPair]
      -> Seq ArgPair
      -> Seq ArgPair
    walk [] defaults
        = defaults
    walk (aug:augs) defaults
        = walk augs $ requireUpdate aug 0 defaults
      where
        requireUpdate
          :: ArgPair
          -> Int
          -> Seq ArgPair
          -> Seq ArgPair
        requireUpdate (name, value) i defs
            | Data.List.null name
            = defs
            | i < Data.Sequence.length defs
            = if argName (Data.Sequence.index defs i) == name
              then Data.Sequence.update i (name, value) defs
              else requireUpdate (name, value) (i + 1) defs
            | otherwise
            = error ("updateArgs: unknown argument: " ++ name)
