module HaskellShell.State (ShellState(..), initializeState, pushHistory) where
import HaskellShell.State.History

data ShellState = ShellState { history :: History
                             --,environment :: Environment
                             }

initializeState :: IO ShellState
initializeState = return $ ShellState { history = emptyHistory 10 }

pushHistory state x = state { history = (addToHistory (history state) x) }

