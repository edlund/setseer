
module Cli where

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

parseArgs
  :: [String]
  -> [ArgPair]
parseArgs []
    = []
parseArgs (arg:args)
    = [transform matches] ++ parseArgs args
  where
    transform
      :: ArgMatch
      -> ArgPair
    transform (left, full, _, matches)
        = (name, value)
      where
        name :: String
        name = if null matches then "" else matches !! 0
        value :: String
        value = if null matches then left else matches !! 2
    pattern :: String
    pattern = "^--([-a-zA-Z0-9]*)(=([-a-zA-Z0-9 ]+))?$"
    matches :: ArgMatch
    matches = arg =~ pattern :: ArgMatch
