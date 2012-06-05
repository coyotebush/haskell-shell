#!/usr/bin/env runhaskell
import Control.Monad
import System.IO
import qualified System.IO.Error as IOE

import HaskellShell.Parse
import HaskellShell.Run

main = do
       shellLoop
       putStrLn "exit"

shellLoop = do
            shellPrompt
            input <- IOE.try (getInput)
            case input of
              Left e ->
                if IOE.isEOFError e
                then return ()
                else ioError e
              Right inStr ->
                do
                runList (parseInput inStr)
                shellLoop

shellPrompt = do
              putStr "$ "
              hFlush stdout

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

