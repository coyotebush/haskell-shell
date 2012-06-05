module HaskellShell.Grammar (Argument, Command, Pipeline, List) where

type Argument = String
type Command = [Argument]
type Pipeline = [Command]
type List = [Pipeline]
