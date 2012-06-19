module HaskellShell.Run (runList) where
import qualified Control.Monad as M
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.Exit
import System.IO
import qualified System.Posix.Process as PP
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
runPipelineElements _  _     [] = return ()
--runPipelineElements input ((cmd, rs):[]) = M.void $ runCommand (P.UseHandle input) (P.UseHandle stdout) (P.UseHandle stderr) cmd
runPipelineElements st input ((cmd, rs):rem)  = do
                                             withStreams defaultStreams rs $
                                           {-nextPipe <- withStream G.Input  rs input  $ \i ->
                                                         withStream G.Output rs stdout $ \o ->
                                                         withStream G.Error  rs stderr $ \e ->-}
                                               runCommand st cmd
                                             runPipelineElements st stdin rem
                                        where defaultStreams = Map.fromList [(0, input), (1, stdout), (2, stderr)]
                                              withStreams :: Map.Map Int Handle -> [G.Redirection] -> (Map.Map Int Handle -> IO ()) -> IO ()
                                              withStreams hs []     f = f hs
                                              withStreams hs ((fds, dest):rs) f = case dest of
                                                                                    G.File path -> withBinaryFile path WriteMode
                                                                                    _           -> undefined
                                                                                  $ \h -> withStreams (insertForKeys fds h hs) rs f
                                              insertForKeys :: Ord k => [k] -> a -> Map.Map k a -> Map.Map k a
                                              insertForKeys [] _ m = m
                                              insertForKeys (k:ks) a m = insertForKeys ks a (Map.insert k a m)
                                      {-where withStream :: G.Stream -> [G.Redirection] -> Handle -> (Handle -> IO a) -> IO a
                                              withStream s rs def f = case lookup s rs of
                                                                        Just G.Pipe -> f undefined
                                                                        Just (G.File path) -> withBinaryFile path WriteMode f
                                                                        Just (G.AppendFile path) -> withBinaryFile path AppendMode f
                                                                        Nothing -> f def-}

--withStreams :: [G.Redirection] -> (Handle -> IO ())

runCommand :: ShellState -> G.Command -> Map.Map Int Handle -> IO ()
runCommand _ [] _  = return ()
runCommand st cmd fds = case lookup (head cmd) builtins of
                       Just builtin -> runBuiltin st (Map.lookup 1 fds) builtin cmd
                       Nothing -> do
                         pid <- PP.forkProcess $ do
                                                 PP.executeFile (head cmd) True (tail cmd) Nothing
                         M.void $ PP.getProcessStatus True False pid
{-
processOpen :: PI.ProcessHandle -> IO Bool
processOpen p = do
                p_ <- PI.withProcessHandle p $ \p_ -> return (p_,p_)
                case p_ of
                  PI.ClosedHandle _ -> return False
                  PI.OpenHandle _   -> return True
-}

