module HaskellShell.Builtins (builtins, runBuiltin) where
import Control.Exception
import System.Exit
import System.Directory
import System.IO
import System.Posix.Process as PP
import qualified System.Process as P (StdStream(..))
import HaskellShell.Error
import HaskellShell.State
import HaskellShell.State.History (getHistory)
import qualified HaskellShell.Grammar as G

type Builtin = ShellState -> Handle -> [G.Argument] -> IO ()

builtins :: [(G.Argument, Builtin)]
builtins = [ ("cd", changeDir)
           , ("pwd", printDir)
           , ("exit", exitShell)
           , ("exec", execCommand)
           , ("history", printHistory)
           ]

runBuiltin :: ShellState -> Handle -> Builtin -> [G.Argument] -> IO ()
runBuiltin st h b (name:args) = handle (shellException [name]) $ b st h args

changeDir st h []      = getHomeDirectory >>= changeDir st h . (:[])
changeDir _ _ (dir:_) = setCurrentDirectory dir

printDir _ h _ = getCurrentDirectory >>= hPutStrLn h

exitShell _ h [] = do
                 hPutStrLn h "exit"
                 exitSuccess

execCommand _ _ (cmd:args) = PP.executeFile cmd True args Nothing

printHistory st h _ = mapM_ printHistoryEntry (getHistory $ history st)
                      where printHistoryEntry (n, s) = hPutStrLn h $ (spacePad 5 $ show n) ++ "  " ++ s
                            spacePad w s = replicate (w - length s) ' ' ++ s


