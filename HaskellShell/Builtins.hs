module HaskellShell.Builtins (builtins, runBuiltin) where
import Control.Exception
import Data.Foldable (toList)
import System.Exit
import System.Directory
import System.IO
import System.Posix.Process as PP
import qualified System.Process as P (StdStream(..))
import HaskellShell.Error
import HaskellShell.History (History(..))
import qualified HaskellShell.Grammar as G

type Builtin = History -> Handle -> [G.Argument] -> IO ()

builtins :: [(G.Argument, Builtin)]
builtins = [ ("cd", changeDir)
           , ("pwd", printDir)
           , ("exit", exitShell)
           , ("exec", execCommand)
           , ("history", printHistory)
           ]

runBuiltin :: History -> Handle -> Builtin -> [G.Argument] -> IO ()
runBuiltin i h b (name:args) = handle (shellException [name]) $ b i h args

changeDir i h []      = getHomeDirectory >>= changeDir i h . (:[])
changeDir _ _ (dir:_) = setCurrentDirectory dir

printDir _ h _ = getCurrentDirectory >>= hPutStrLn h

exitShell _ h [] = do
                 hPutStrLn h "exit"
                 exitSuccess

execCommand _ _ (cmd:args) = PP.executeFile cmd True args Nothing

printHistory i h _ = mapM_ (hPutStrLn h) (toList $ entries i)


