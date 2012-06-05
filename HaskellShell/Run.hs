module HaskellShell.Run (runList) where
import HaskellShell.Builtins

runList = mapM runPipeline

runPipeline = mapM runCommand

runCommand cmd | length cmd > 0 = do
                                  case lookup (head cmd) builtins of
                                    Just builtin -> builtin (tail cmd)
                                    Nothing -> return ()

