module HaskellShell.Run (runList) where
import qualified Control.Monad as M
import System.IO (stdin, stdout, stderr)
import qualified System.Process as P
import HaskellShell.Builtins
import qualified HaskellShell.Grammar as G

runList :: G.List -> IO ()
runList = mapM_ runPipeline

runPipeline :: G.Pipeline -> IO ()
runPipeline = mapM_ (runCommand (P.UseHandle stdin) (P.UseHandle stdout) (P.UseHandle stderr) . snd)

--runPipelineElement :: G.PipelineElement -> 

runCommand :: P.StdStream -> P.StdStream -> P.StdStream -> G.Command -> IO ()
runCommand _ _ _ []  = return ()
runCommand i o e cmd = case lookup (head cmd) builtins of
                       Just builtin -> runBuiltin o builtin cmd
                       Nothing -> do
                         (_, _, _, p) <- P.createProcess $
                           (P.proc (head cmd) (tail cmd)) { P.std_in  = i
                                                          , P.std_out = o
                                                          , P.std_err = e
                                                          }
                         M.void $ P.waitForProcess p

