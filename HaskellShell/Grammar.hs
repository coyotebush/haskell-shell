module HaskellShell.Grammar (Argument, Command, Pipe(..), Pipeline, List) where

type Argument = String
type Command = [Argument]
data Pipe = Pipe | NoPipe deriving (Eq, Show)
type PipelineElement = (Pipe, Command)
type Pipeline = [PipelineElement]
type List = [Pipeline]
