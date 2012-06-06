module HaskellShell.Builtins (builtins, runBuiltin) where
import Control.Exception
import System.Exit
import System.Directory
import System.IO
import System.Posix.Process as PP
import qualified System.Process as P (StdStream(..))
import HaskellShell.Error
import qualified HaskellShell.Grammar as G

type Builtin = Handle -> [G.Argument] -> IO ()

builtins :: [(G.Argument, Builtin)]
builtins = [ ("cd", changeDir)
           , ("pwd", printDir)
           , ("exit", exitShell)
           , ("exec", execCommand)
           ]

runBuiltin :: Handle -> Builtin -> [G.Argument] -> IO ()
runBuiltin h b (name:args) = handle (shellException [name]) $ b h args

changeDir h []      = getHomeDirectory >>= changeDir h . (:[])
changeDir _ (dir:_) = setCurrentDirectory dir

printDir h _ = getCurrentDirectory >>= hPutStrLn h

exitShell h [] = do
                 hPutStrLn h "exit"
                 exitSuccess

execCommand _ (cmd:args) = PP.executeFile cmd True args Nothing

