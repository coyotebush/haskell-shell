module HaskellShell.Parse (parseInput) where
import qualified Data.List.Split as S
import qualified HaskellShell.Grammar as G
import HaskellShell.Parse.Lex

parseInput :: String -> G.List
parseInput = parseList . dropBlanks . lexInput

parseList :: [ShellToken] -> G.List
parseList = filter (/= [[]]) . map parsePipeline . S.splitWhen (isOperator listOperators)

parsePipeline :: [ShellToken] -> G.Pipeline
parsePipeline = (:[]) . parseCommand

parseCommand :: [ShellToken] -> [G.Argument]
parseCommand = map showToken

