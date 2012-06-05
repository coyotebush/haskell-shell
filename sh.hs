#!/usr/bin/env runhaskell
import Control.Monad
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
                let lists = parseInput inStr
                mapM runPipeline lists
                shellLoop

shellPrompt = do
              putStr "$ "
              hFlush stdout

runPipeline = mapM runCommand

runCommand cmd | length cmd > 0 = do
                                  case lookup (head cmd) builtins of
                                    Just builtin -> builtin (tail cmd)
                                    Nothing -> return ()

