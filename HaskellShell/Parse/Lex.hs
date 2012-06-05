module HaskellShell.Parse.Lex (ShellToken(..), lexInput) where

data ShellToken = Blank
                | Word String
                | Operator String
                | Quote Char String
                deriving Show

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

-- from dash(1) man page
operators = [ "&",  "&&", "(",  ")",  ";",  ";;", "|",  "||"
            , "<",  ">",  ">|", "<<", ">>", "<&", ">&", "<<-", "<>" ]

