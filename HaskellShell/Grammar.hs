module HaskellShell.Grammar (Argument, Command, Pipe(..), Pipeline, List) where

type Argument = String
type Command = [Argument]
data Pipe = Pipe | NoPipe deriving (Eq, Show)
type Pipeline = [(Pipe, Command)]
type List = [Pipeline]
