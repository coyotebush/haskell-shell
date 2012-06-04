#!/usr/bin/env runhaskell
import System.Directory
import System.IO
import System.IO.Error

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
                case lookup inStr builtins of
                  Just builtin -> builtin
                  Nothing -> return ()
                shellLoop

shellPrompt = do
              putStr "$ "
              hFlush stdout

builtins = [ ("cd", changeDir)
           , ("pwd", printDir)
           ]

changeDir = getHomeDirectory >>= setCurrentDirectory

printDir = getCurrentDirectory >>= putStrLn

