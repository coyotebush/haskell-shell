module HaskellShell.Run (runList) where
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
                                    Nothing -> return ()

