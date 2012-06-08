module HaskellShell.Parse (parseInput) where
import qualified Data.List.Split as S
import qualified HaskellShell.Grammar as G
import HaskellShell.Parse.Lex

parseInput :: String -> G.List
parseInput = parseList . flattenQuotes . dropBlanks . lexInput

parseList :: [ShellToken] -> G.List
parseList = filter (/= [([], [])]) . map parsePipeline . S.splitWhen (isOperator listOperators)

parsePipeline :: [ShellToken] -> G.Pipeline
parsePipeline = map parsePipelineElement . S.split (S.keepDelimsR $ (S.whenElt (isOperator pipelineOperators)))

parsePipelineElement :: [ShellToken] -> G.PipelineElement
parsePipelineElement = (\(cmd, rs) -> (parseCommand cmd, parseRedirections rs)) . break (isOperator redirectionOperators)
                          
parseRedirections :: [ShellToken] -> [G.Redirection]
parseRedirections ((Operator "|" ):xs) = (G.Output, G.Pipe)
                                         : parseRedirections xs
parseRedirections ((Operator "|&"):xs) = (G.Output, G.Pipe) : (G.Error, G.Pipe)
                                         : parseRedirections xs
parseRedirections ((Operator ">" ):(Word s):xs) = (G.Output, G.File s)
                                                  : parseRedirections xs
parseRedirections ((Operator ">>"):(Word s):xs) = (G.Output, G.AppendFile s)
                                                  : parseRedirections xs
parseRedirections ((Operator ">&"):(Word s):xs) = (G.Output, G.File s) : (G.Error, G.File s)
                                                  : parseRedirections xs
parseRedirections (_:xs) = parseRedirections xs
parseRedirections []     = []

{-
parsePipeOperator :: ShellToken -> G.Pipe
parsePipeOperator (Operator "|") = G.Pipe
parsePipeOperator _              = G.NoPipe
-}

parseCommand :: [ShellToken] -> [G.Argument]
parseCommand = map showToken

flattenQuotes :: [ShellToken] -> [ShellToken]
flattenQuotes [] = []
flattenQuotes ((Quote _ s):xs) = (Word s) : flattenQuotes xs
flattenQuotes (x:xs) = x : flattenQuotes xs

