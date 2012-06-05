module HaskellShell.Parse (parseInput) where
import qualified HaskellShell.Grammar as G

parseInput :: String -> G.List
parseInput (';':s) = parseInput s
parseInput s = case break (== ';') s of
                 ("", "") -> []
                 (cmdStr, rem) -> ([parseCommand cmdStr]) : parseInput rem

parseCommand :: String -> [G.Argument]
parseCommand = words

