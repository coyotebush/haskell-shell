#!/usr/bin/env runhaskell
import System.IO
import qualified System.IO.Error as IOE

import HaskellShell.Input
import HaskellShell.Parse
import HaskellShell.Run

main = shellLoop

shellLoop = do
            input <- IOE.try promptInput
            case input of
              Left e | IOE.isEOFError e -> putStrLn "exit"
                     | otherwise        -> ioError e
              Right inStr -> do
                             runList $ parseInput inStr
                             shellLoop

