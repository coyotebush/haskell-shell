module HaskellShell.Run (runList) where
import Control.Exception (bracket)
import qualified Control.Monad as M
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.Exit
import System.IO
import qualified System.Posix.Process as PP
import System.Posix.IO
import System.Posix.Types (Fd, ProcessID)
import HaskellShell.Builtins
import HaskellShell.Error
import qualified HaskellShell.Grammar as G
import HaskellShell.State
import HaskellShell.State.History (History)

runList :: ShellState -> G.List -> IO ()
runList st = mapM_ (runPipeline st)

runPipeline :: ShellState -> G.Pipeline -> IO ()
runPipeline st xs = do
                 pids <- runPipelineElements st (stdin, False) xs
                 mapM_ (PP.getProcessStatus True False) pids
--runPipeline = mapM_ (runCommand (P.P.UseHandle stdin) (P.P.UseHandle stdout) (P.P.UseHandle stderr) . snd)

runPipelineElements :: ShellState -> (Handle, Bool) -> [G.PipelineElement] -> IO [ProcessID]
runPipelineElements _  (input, True)  [] = do
                                           closeHandle input
                                           return []
runPipelineElements _  _              [] = return []
--runPipelineElements input ((cmd, rs):[]) = M.void $ runCommand (P.UseHandle input) (P.UseHandle stdout) (P.UseHandle stderr) cmd
runPipelineElements st (input, close) ((cmd, rs):rem)  = do
                                             next <- withStreams defaultStreams rs $ runCommand st cmd
                                             M.when close (closeHandle input)
                                             pids <- runPipelineElements st (case snd next of Just h -> (h, True); Nothing -> (stdin, False)) rem
                                             return $ case fst next of Just pid -> pid:pids; Nothing -> pids
                                        where defaultStreams = Map.fromList [(stdInput, input), (stdOutput, stdout), (stdError, stderr)]
                                              withStreams :: Map.Map Fd Handle -> [G.Redirection] -> (Map.Map Fd Handle -> IO a) -> IO (a, Maybe Handle)
                                              withStreams hs []     f = do
                                                                        r <- f hs
                                                                        return (r, Nothing)
                                              withStreams hs ((fds, dest):rs) f = case dest of
                                                                                    G.File       path -> withBinaryFile path WriteMode  . \f x -> f (Nothing, x)
                                                                                    G.AppendFile path -> withBinaryFile path AppendMode . \f x -> f (Nothing, x)
                                                                                    G.Pipe            -> withPipe
                                                                                  $ \(out, h) -> do
                                                                                              (r, _) <- withStreams (insertForKeys fds h hs) rs f
                                                                                              return (r, out)
                                              withPipe :: ((Maybe Handle, Handle) -> IO a) -> IO a
                                              withPipe = bracket (do
                                                                    (pr, pw) <- createPipe
                                                                    prh <- fdToHandle pr
                                                                    pwh <- fdToHandle pw
                                                                    return (Just prh, pwh))
                                                                   (closeHandle . snd)
                                              insertForKeys :: Ord k => [k] -> a -> Map.Map k a -> Map.Map k a
                                              insertForKeys [] _ m = m
                                              insertForKeys (k:ks) a m = insertForKeys ks a (Map.insert k a m)

closeHandle :: Handle -> IO ()
closeHandle h = handleToFd h >>= closeFd
                                      {-where withStream :: G.Stream -> [G.Redirection] -> Handle -> (Handle -> IO a) -> IO a
                                              withStream s rs def f = case lookup s rs of
                                                                        Just G.Pipe -> f undefined
                                                                        Just (G.File path) -> withBinaryFile path WriteMode f
                                                                        Just (G.AppendFile path) -> withBinaryFile path AppendMode f
                                                                        Nothing -> f def-}

--withStreams :: [G.Redirection] -> (Handle -> IO ())

runCommand :: ShellState -> G.Command -> Map.Map Fd Handle -> IO (Maybe ProcessID)
runCommand _ [] _  = return Nothing
runCommand st cmd fds = case lookup (head cmd) builtins of
                       Just builtin -> do
                         runBuiltin st (Map.lookup stdOutput fds) builtin cmd
                         return Nothing
                       Nothing -> do
                         pid <- PP.forkProcess $ do
                                                 Map.traverseWithKey (\i h -> handleToFd h >>= flip dupTo i) fds
                                                 PP.executeFile (head cmd) True (tail cmd) Nothing
                         return (Just pid)
{-
processOpen :: PI.ProcessHandle -> IO Bool
processOpen p = do
                p_ <- PI.withProcessHandle p $ \p_ -> return (p_,p_)
                case p_ of
                  PI.ClosedHandle _ -> return False
                  PI.OpenHandle _   -> return True
-}

