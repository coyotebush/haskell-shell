#!/usr/bin/env runhaskell
import System.Directory
import System.IO
import System.IO.Error

main = do
       shellLoop
       putStrLn "exit"

shellLoop = do
            putStr "$ "
            hFlush stdout
            input <- try (getLine)
            case input of
              Left e ->
                if isEOFError e
                then return ()
                else ioError e
              Right inStr ->
                do
                case inStr of
                  "cd"  -> do
                           dir <- getHomeDirectory
                           setCurrentDirectory dir
                  "pwd" -> do
                           dir <- getCurrentDirectory
                           putStrLn dir
                  ""    -> return ()
                shellLoop

