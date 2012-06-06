#!/usr/bin/env runhaskell
import System.IO
import qualified System.IO.Error as IOE

import HaskellShell.History
import HaskellShell.Input
import HaskellShell.Parse
import HaskellShell.Run

main = shellLoop $ emptyHistory 10

shellLoop :: History -> IO ()
shellLoop hist = do
                 input <- IOE.try promptInput
                 case input of
                   Left e | IOE.isEOFError e -> putStrLn "exit"
                          | otherwise        -> ioError e
                   Right inStr -> do
                                  runList $ parseInput inStr
                                  shellLoop (addToHistory hist inStr)

