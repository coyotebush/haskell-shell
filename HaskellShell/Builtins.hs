module HaskellShell.Builtins (builtins) where
import System.Directory

builtins = [ ("cd", changeDir)
           , ("pwd", printDir)
           ]

changeDir []      = getHomeDirectory >>= setCurrentDirectory
changeDir (dir:_) = setCurrentDirectory dir

printDir _ = getCurrentDirectory >>= putStrLn

