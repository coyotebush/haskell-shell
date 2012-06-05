module HaskellShell.Parse.Lex (ShellToken(..), lexInput) where

data ShellToken = Blank
                | NormalToken String
                | ControlToken String
                | Quote Char String
                deriving Show

lexInput :: String -> [ShellToken]
lexInput "" = []
lexInput (c:rem) | isSpace c       = Blank : (lexInput rem)
                 | isQuote c       = let (cs, _:rest) = break (== c) rem in
                                     (Quote c cs) : (lexInput rest)
                 | isControlChar c = let (cs, rest) = span isControlChar rem in
                                     (ControlToken (c:cs)) : (lexInput rest)
                 | otherwise       = let (cs, rest) = break (\x -> isSpace x || isQuote x || isControlChar x) rem in
                                     (NormalToken (c:cs)) : (lexInput rest)

isSpace = (==) ' '
isQuote = flip elem "'\"`"
isControlChar = flip elem "|&;()<>"
