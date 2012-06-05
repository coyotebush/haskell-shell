module HaskellShell.Builtins (builtins, runBuiltin) where
import Control.Exception
import System.Exit
import System.Directory
import HaskellShell.Error
import qualified HaskellShell.Grammar as G

type Builtin = [G.Argument] -> IO ()

builtins :: [(G.Argument, Builtin)]
builtins = [ ("cd", changeDir)
           , ("pwd", printDir)
           , ("exit", exitShell)
           ]

runBuiltin :: Builtin -> [G.Argument] -> IO ()
runBuiltin b (name:args) = handle (shellException [name]) $ b args

changeDir []      = getHomeDirectory >>= changeDir . (:[])
changeDir (dir:_) = setCurrentDirectory dir

printDir _ = getCurrentDirectory >>= putStrLn

exitShell [] = do
               putStrLn "exit"
               exitSuccess

