module HaskellShell.Grammar where

type Argument = String
type Command = [Argument]
data Stream = Input | Output | Error
              deriving (Eq, Show)
data Destination = Inherit | Pipe | File FilePath | AppendFile FilePath
                   deriving (Eq, Show)
type Redirection = (Stream, Destination)
type PipelineElement = (Command, [Redirection])
type Pipeline = [PipelineElement]
type List = [Pipeline]
