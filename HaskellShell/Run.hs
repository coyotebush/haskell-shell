module HaskellShell.Run (runList) where
import qualified Control.Monad as M
import Data.Maybe (fromJust)
import System.Exit
import System.IO (stdin, stdout, stderr, Handle)
import qualified System.Process as P
import System.Posix.IO (createPipe, fdToHandle, handleToFd, closeFd)
import HaskellShell.Builtins
import HaskellShell.Error
import qualified HaskellShell.Grammar as G
import HaskellShell.State
import HaskellShell.State.History (History)

runList :: ShellState -> G.List -> IO ()
runList st = mapM_ (runPipeline st)

runPipeline :: ShellState -> G.Pipeline -> IO ()
runPipeline st = runPipelineElements st stdin
--runPipeline = mapM_ (runCommand (P.P.UseHandle stdin) (P.P.UseHandle stdout) (P.P.UseHandle stderr) . snd)

runPipelineElements :: ShellState -> Handle -> [G.PipelineElement] -> IO ()
runPipelineElements st input ((cmd, G.NoPipe):[]) = M.void $ runCommand st (P.UseHandle input) (P.UseHandle stdout) (P.UseHandle stderr) cmd
runPipelineElements st input ((cmd, G.Pipe):rem)  = do
                                                 nextPipe <- runCommand st (P.UseHandle input) P.CreatePipe (P.UseHandle stderr) cmd
                                                 case nextPipe of
                                                   Just ph -> runPipelineElements st ph rem
                                                   Nothing -> runPipelineElements st stdin rem

runCommand :: ShellState -> P.StdStream -> P.StdStream -> P.StdStream -> G.Command -> IO (Maybe Handle)
runCommand _ _ _ _ []  = return Nothing
runCommand st i o e cmd = case lookup (head cmd) builtins of
                       Just builtin -> case o of
                                         (P.UseHandle h) -> do
                                                            runBuiltin st h builtin cmd
                                                            return Nothing
                                         P.CreatePipe    -> do
                                                            (fr, fw) <- createPipe
                                                            hr <- fdToHandle fr
                                                            hw <- fdToHandle fw
                                                            runBuiltin st hw builtin cmd
                                                            handleToFd hw >>= closeFd
                                                            return (Just hr)
                       Nothing -> do
                         (_, out, _, p) <- P.createProcess $
                           (P.proc (head cmd) (tail cmd)) { P.std_in  = i
                                                          , P.std_out = o
                                                          , P.std_err = e
                                                          }
                         code <- P.waitForProcess p
                         case code of
                           ExitFailure 127 -> shellError [(head cmd), "command not found"]
                           _               -> return ()
                         return out
{-
processOpen :: PI.ProcessHandle -> IO Bool
processOpen p = do
                p_ <- PI.withProcessHandle p $ \p_ -> return (p_,p_)
                case p_ of
                  PI.ClosedHandle _ -> return False
                  PI.OpenHandle _   -> return True
-}

