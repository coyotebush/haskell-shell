#!/usr/bin/env runhaskell
import System.IO
import qualified System.IO.Error as IOE

import HaskellShell.Input
import HaskellShell.Parse
import HaskellShell.Run
import HaskellShell.State

main = initializeState >>= shellLoop

shellLoop :: ShellState -> IO ()
shellLoop st = do
               input <- IOE.try promptInput
               case input of
                 Left e | IOE.isEOFError e -> putStrLn "exit"
                        | otherwise        -> ioError e
                 Right inStr -> do
                                runList st $ parseInput inStr
                                shellLoop (pushHistory st inStr)

