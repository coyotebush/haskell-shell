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
               input <- IOE.tryIOError promptInput
               case input of
                 Left e | IOE.isEOFError e -> putStrLn "exit"
                        | otherwise        -> ioError e
                 Right inStr -> do
                                newState <- pushHistory st inStr
                                runList newState $ parseInput inStr
                                shellLoop newState

