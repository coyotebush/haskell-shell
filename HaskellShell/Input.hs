module HaskellShell.Input (promptInput) where
import System.IO
import System.Posix.Signals

promptInput = do 
              installHandler keyboardSignal (Catch newShellPrompt) Nothing
              shellPrompt
              input <- getInput
              installHandler keyboardSignal (Catch (putStrLn "")) Nothing
              return input

shellPrompt = do
              putStr "$ "
              hFlush stdout

newShellPrompt = do
                 putStrLn ""
                 shellPrompt

getInput :: IO String
getInput = do
           line <- getLine
           if not (null line) && last line == '\\'
           then do
                secondaryPrompt
                next <- getInput
                return (init line ++ next)
           else return line

secondaryPrompt = do
                  putStr "> "
                  hFlush stdout

