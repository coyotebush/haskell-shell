module HaskellShell.Builtins (builtins) where
import Control.Exception
import System.Exit
import System.Directory
import System.IO.Error
import qualified HaskellShell.Grammar as G

builtins :: [(G.Argument, [G.Argument] -> IO ())]
builtins = [ ("cd", changeDir)
           , ("pwd", printDir)
           , ("exit", exitShell)
           ]

changeDir []      = getHomeDirectory >>= setCurrentDirectory
changeDir (dir:_) = handle handler $ setCurrentDirectory dir
                    where handler e | isDoesNotExistError e
                            = putStrLn $ "sh.hs: cd: " ++ dir ++ ": No such file or directory"
                          handler _
                            = putStrLn "sh.hs: cd: error"

printDir _ = getCurrentDirectory >>= putStrLn

exitShell [] = do
               putStrLn "exit"
               exitSuccess

