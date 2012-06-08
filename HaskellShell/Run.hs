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

runList :: G.List -> IO ()
runList = mapM_ runPipeline

runPipeline :: G.Pipeline -> IO ()
runPipeline = runPipelineElements stdin
--runPipeline = mapM_ (runCommand (P.P.UseHandle stdin) (P.P.UseHandle stdout) (P.P.UseHandle stderr) . snd)

runPipelineElements :: Handle -> [G.PipelineElement] -> IO ()
runPipelineElements input ((cmd, G.NoPipe):[]) = M.void $ runCommand (P.UseHandle input) (P.UseHandle stdout) (P.UseHandle stderr) cmd
runPipelineElements input ((cmd, G.Pipe):rem)  = do
                                                 nextPipe <- runCommand (P.UseHandle input) P.CreatePipe (P.UseHandle stderr) cmd
                                                 case nextPipe of
                                                   Just ph -> runPipelineElements ph rem
                                                   Nothing -> runPipelineElements stdin rem

runCommand :: P.StdStream -> P.StdStream -> P.StdStream -> G.Command -> IO (Maybe Handle)
runCommand _ _ _ []  = return Nothing
runCommand i o e cmd = case lookup (head cmd) builtins of
                       Just builtin -> case o of
                                         (P.UseHandle h) -> do
                                                            runBuiltin h builtin cmd
                                                            return Nothing
                                         P.CreatePipe    -> do
                                                            (fr, fw) <- createPipe
                                                            hr <- fdToHandle fr
                                                            hw <- fdToHandle fw
                                                            runBuiltin hw builtin cmd
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

