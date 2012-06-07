module HaskellShell.State (ShellState(..), initializeState, pushHistory) where
import HaskellShell.State.History
import HaskellShell.State.Environment

data ShellState = ShellState { history :: History
                             , environment :: Environment
                             }
                  deriving Show

initializeState :: IO ShellState
initializeState = do 
                  env <- initializeEnvironment
                  return $ ShellState { history = emptyHistory 10
                                      , environment = env
                                      }

pushHistory state x = state { history = (addToHistory (history state) x) }

