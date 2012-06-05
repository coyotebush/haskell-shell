module HaskellShell.Parse (parseInput) where

type Arg = String

parseInput :: String -> [Arg]
parseInput = words

