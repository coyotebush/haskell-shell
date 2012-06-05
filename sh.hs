#!/usr/bin/env runhaskell
import System.IO
import System.IO.Error

import HaskellShell.Builtins
import HaskellShell.Parse

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
                let args = parseInput inStr
                if length args > 0 then
                  case lookup (head args) builtins of
                    Just builtin -> builtin (tail args)
                    Nothing -> return ()
                else return ()
                shellLoop

shellPrompt = do
              putStr "$ "
              hFlush stdout

