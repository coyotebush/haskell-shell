module HaskellShell.Run (runList) where
import qualified Control.Monad as M
import qualified System.Process as P
import HaskellShell.Builtins
import qualified HaskellShell.Grammar as G

runList :: G.List -> IO ()
runList = mapM_ runPipeline

runPipeline :: G.Pipeline -> IO ()
runPipeline = mapM_ (runCommand . snd)

runCommand :: G.Command -> IO ()
runCommand [] = return ()
runCommand cmd = do
                 case lookup (head cmd) builtins of
                   Just builtin -> runBuiltin builtin cmd
                   Nothing -> do
                     (_, _, _, p) <- P.createProcess
                       (P.proc (head cmd) (tail cmd))
                     M.void $ P.waitForProcess p

