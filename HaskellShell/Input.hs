module HaskellShell.Input (getInput) where
import System.IO
import Control.Monad

getInput :: IO String
getInput = do
           line <- getLine
           case line of
             "" -> return line
             _  -> case last line of
                    '\\' -> do
                       secondaryPrompt
                       next <- getInput
                       return (init line ++ next)
                    _    -> return line

secondaryPrompt = do
                  putStr "> "
                  hFlush stdout

