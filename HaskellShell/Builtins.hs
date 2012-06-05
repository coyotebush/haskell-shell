module HaskellShell.Builtins (builtins) where
import System.Directory
import qualified HaskellShell.Grammar as G

builtins :: [(G.Argument, [G.Argument] -> IO ())]
builtins = [ ("cd", changeDir)
           , ("pwd", printDir)
           ]

changeDir []      = getHomeDirectory >>= setCurrentDirectory
changeDir (dir:_) = setCurrentDirectory dir

printDir _ = getCurrentDirectory >>= putStrLn

