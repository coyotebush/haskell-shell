module HaskellShell.Grammar where
import System.Posix.Types (Fd)

type Argument = String
type Command = [Argument]
type Stream = Fd
data Destination = Pipe | File FilePath | AppendFile FilePath
                   deriving (Eq, Show)
type Redirection = ([Stream], Destination)
type PipelineElement = (Command, [Redirection])
type Pipeline = [PipelineElement]
type List = [Pipeline]
