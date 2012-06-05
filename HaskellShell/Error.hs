module HaskellShell.Error (shellError, shellException) where
import Control.Exception
import Data.List (intercalate)
import Data.Maybe
import System.IO.Error

shellName = "sh.hs"

shellError :: [String] -> IO ()
shellError ss = putStrLn $ intercalate ": " (shellName : ss)

shellException :: [String] -> IOException -> IO ()
shellException ss e = shellError (ss ++ errorMessage e)

errorMessage :: IOException -> [String]
errorMessage e | isDoesNotExistError e = catMaybes [ioeGetFileName e, Just "No such file or directory"] 
errorMessage _ = ["Error"]
