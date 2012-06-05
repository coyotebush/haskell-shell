module HaskellShell.Run (runList) where
import qualified Control.Monad as M
import qualified System.Process as P
import HaskellShell.Builtins
import qualified HaskellShell.Grammar as G

runList :: G.List -> IO ()
runList = mapM_ runPipeline

runPipeline :: G.Pipeline -> IO ()
runPipeline = mapM_ runCommand

runCommand :: G.Command -> IO ()
runCommand cmd | length cmd > 0 = do
                                  case lookup (head cmd) builtins of
                                    Just builtin -> builtin (tail cmd)
                                    Nothing -> do
                                               (_, _, _, p) <- P.createProcess
                                                 (P.proc (head cmd) (tail cmd))
                                               M.void $ P.waitForProcess p

