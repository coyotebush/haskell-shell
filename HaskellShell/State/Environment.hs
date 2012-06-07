module HaskellShell.State.Environment (Environment, initializeEnvironment) where
import Data.Map
import System.Environment

type Environment = Map String String
initializeEnvironment :: IO Environment
initializeEnvironment = getEnvironment >>= return . union defaultEnvironment . fromList

defaultEnvironment = fromList [ ("HISTSIZE", "10")
                              ]

