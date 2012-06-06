module HaskellShell.Parse.Lex
     ( ShellToken(..)
     , showToken
     , dropBlanks
     , lexInput
     , listOperators
     , pipelineOperators
     , redirectionOperators
     , isOperator
     ) where

data ShellChar = Escaped Char | Unescaped Char deriving (Eq, Show)

data ShellToken = Blank
                | Word String
                | Operator String
                | Quote Char String
                deriving Show

showToken :: ShellToken -> String
showToken Blank        = " "
showToken (Word s)     = s
showToken (Operator s) = s
showToken (Quote c s)  = s

escapes :: String -> [ShellChar]
escapes []            = []
escapes ('\\':c:rest) = Escaped c : escapes rest
escapes (c:rest)      = Unescaped c : escapes rest

unescape :: ShellChar -> Char
unescape (Escaped c)   = c
unescape (Unescaped c) = c

lexInput :: String -> [ShellToken]
lexInput "" = []
lexInput (c:rem) | isSpace c            = Blank : (lexInput rem)
                 | isQuote c            = let (cs, _:rest) = break (== c) rem in
                                          (Quote c cs) : (lexInput rest)
                 | [c] `elem` operators = let (o, rest) = takeOperator [c] rem in
                                          (Operator o) : (lexInput rest)
                 | otherwise            = let (cs, rest) = break (\x -> isSpace x || isQuote x || [x] `elem` operators) rem in
                                     (Word (c:cs)) : (lexInput rest)

takeOperator :: String -> String -> (String, String)
takeOperator o []     = (o, "")
takeOperator o (x:xs) = let n = o ++ [x] in
                        if n `elem` operators
                        then takeOperator n xs
                        else (o, x:xs)

isSpace = (==) ' '
isQuote = flip elem "'\"`"

-- based on bash(1) and dash(1) man pages
listOperators = [ ";", "&", "&&", "||" ]
pipelineOperators = [ "|", "|&" ]
redirectionOperators = [ "<", ">", ">|", "<<", ">>", "<&", ">&", "<<-", "<>" ]
operators = listOperators ++ pipelineOperators ++ redirectionOperators -- "(", ")", ";;"

isOperator :: [String] -> ShellToken -> Bool
isOperator ops (Operator s) = s `elem` ops
isOperator _   _            = False

dropBlanks = filter isNotBlank
             where isNotBlank Blank = False
                   isNotBlank _     = True

