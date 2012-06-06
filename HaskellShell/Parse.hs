module HaskellShell.Parse (parseInput) where
import qualified Data.List.Split as S
import qualified HaskellShell.Grammar as G
import HaskellShell.Parse.Lex

parseInput :: String -> G.List
parseInput = parseList . dropBlanks . lexInput

parseList :: [ShellToken] -> G.List
parseList = filter (/= [([], G.NoPipe)]) . map parsePipeline . S.splitWhen (isOperator listOperators)

parsePipeline :: [ShellToken] -> G.Pipeline
parsePipeline = mapLast
                  (\c -> (parseCommand c, G.NoPipe))
                  (\c -> (parseCommand (init c), parsePipeOperator (last c)))
                . S.split (S.keepDelimsR $ (S.whenElt (isOperator pipelineOperators)))

mapTail :: (a -> b) -> (a -> b) -> [a] -> [b]
mapTail f1 f2 xs = f1 (head xs) : map f2 (tail xs)

mapLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapLast f1 f2 xs = map f2 (init xs) ++ [f1 (last xs)]

parsePipeOperator :: ShellToken -> G.Pipe
parsePipeOperator (Operator "|") = G.Pipe
parsePipeOperator _              = G.NoPipe

parseCommand :: [ShellToken] -> [G.Argument]
parseCommand = map showToken

