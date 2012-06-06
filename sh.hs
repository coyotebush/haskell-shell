#!/usr/bin/env runhaskell
import System.IO
import qualified System.IO.Error as IOE

import HaskellShell.Input
import HaskellShell.Parse
import HaskellShell.Run

main = do
       shellLoop
       putStrLn "exit"

shellLoop = do
            input <- IOE.try promptInput
            case input of
              Left e | IOE.isEOFError e -> return ()
                     | otherwise -> ioError e
              Right inStr -> do
                             runList $ parseInput inStr
                             shellLoop

