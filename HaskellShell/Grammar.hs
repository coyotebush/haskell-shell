module HaskellShell.Grammar (Argument, Command, Pipe(..), Pipeline, List) where

type Argument = String
type Command = [Argument]
data Pipe = Pipe | NoPipe deriving (Eq, Show)
type PipelineElement = (Command, Pipe)
type Pipeline = [PipelineElement]
type List = [Pipeline]
