#!/usr/bin/env runhaskell
import Control.Monad
import System.IO
import System.IO.Error

import HaskellShell.Parse
import HaskellShell.Run

main = do
       shellLoop
       putStrLn "exit"

shellLoop = do
            shellPrompt
            input <- try (getLine)
            case input of
              Left e ->
                if isEOFError e
                then return ()
                else ioError e
              Right inStr ->
                do
                runList (parseInput inStr)
                shellLoop

shellPrompt = do
              putStr "$ "
              hFlush stdout

