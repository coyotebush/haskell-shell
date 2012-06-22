module HaskellShell.Error (shellError, shellException) where
import Control.Exception
import Data.List (intercalate)
import Data.Maybe
import System.IO
import System.IO.Error

shellName = "sh.hs"

shellError :: Handle -> [String] -> IO ()
shellError h ss = hPutStrLn h $ intercalate ": " (shellName : ss)

shellException :: Handle -> [String] -> IOException -> IO ()
shellException h ss e = shellError h (ss ++ errorMessage e)

errorMessage :: IOException -> [String]
errorMessage e | isDoesNotExistError e = maybeToList (ioeGetFileName e) ++ ["No such file or directory"]
errorMessage _ = ["Error"]
