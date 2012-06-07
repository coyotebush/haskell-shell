module HaskellShell.State (ShellState(..), initializeState, pushHistory) where
import System.Posix.Env
import HaskellShell.State.History
import HaskellShell.State.Environment

data ShellState = ShellState { history :: History
                             }
                  deriving Show

initializeState :: IO ShellState
initializeState = do
                  initializeEnvironment
                  return $ ShellState { history = emptyHistory }

pushHistory :: ShellState -> String -> IO ShellState
pushHistory state x = do
                      depth <- readEnv "HISTSIZE"
                      return $ state { history = (addToHistory (history state) depth x) }

