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

isEscaped :: ShellChar -> Bool
isEscaped (Escaped _) = True
isEscaped (Unescaped _) = False

unescape :: ShellChar -> Char
unescape (Escaped c)   = c
unescape (Unescaped c) = c

literalUnescape :: ShellChar -> String
literalUnescape (Escaped c)   = ['\\', c]
literalUnescape (Unescaped c) = [c]

isChar :: ShellChar -> Char -> Bool
isChar (Unescaped c) c2 = c == c2
isChar (Escaped _)   _  = False

isAnyChar :: [Char] -> ShellChar -> Bool
isAnyChar cs sc = any (isChar sc) cs

lexInput :: String -> [ShellToken]
lexInput = lexChars . escapes

lexChars :: [ShellChar] -> [ShellToken]
lexChars [] = []
lexChars (c:rem) | isAnyChar spaces c  = Blank : (lexChars rem)
                 | isAnyChar quotes c  = let (cs, _:rest) = break (== c) rem in
                                         (Quote (unescape c) (concatMap literalUnescape cs)) : (lexChars rest)
                 | isAnyChar opChars c = let (o, rest) = takeOperator' (c:rem) in
                                         (Operator o) : (lexChars rest)
                 | otherwise           = let (cs, rest) = break (isAnyChar (spaces ++ quotes ++ opChars)) rem in
                                         (Word $ map unescape (c:cs)) : (lexChars rest)

takeOperator :: String -> String -> (String, String)
takeOperator o []     = (o, "")
takeOperator o (x:xs) = let n = o ++ [x] in
                        if n `elem` operators
                        then takeOperator n xs
                        else (o, x:xs)

takeOperator' :: [ShellChar] -> (String, [ShellChar])
takeOperator' (x:xs) = let (u, r1) = break isEscaped xs
                           (o, r2) = takeOperator [(unescape x)] (map unescape u)
                       in  (o, r1 ++ (map Unescaped r2))

spaces = " "
quotes = "'\"`"

-- based on bash(1) and dash(1) man pages
listOperators = [ ";", "&", "&&", "||" ]
pipelineOperators = [ "|", "|&" ]
redirectionOperators = [ "<", ">", ">|", "<<", ">>", "<&", ">&", "<<-", "<>" ]
operators = listOperators ++ pipelineOperators ++ redirectionOperators -- "(", ")", ";;"
opChars = map head operators

isOperator :: [String] -> ShellToken -> Bool
isOperator ops (Operator s) = s `elem` ops
isOperator _   _            = False

dropBlanks = filter isNotBlank
             where isNotBlank Blank = False
                   isNotBlank _     = True

