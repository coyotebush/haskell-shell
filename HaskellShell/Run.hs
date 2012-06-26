module HaskellShell.Run (runList) where

import Control.Exception (bracket, catch, IOException)
import qualified Control.Monad as M
import Data.Maybe

import System.Exit
import System.IO
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types (Fd, ProcessID)

import HaskellShell.Builtins
import HaskellShell.Error
import qualified HaskellShell.Grammar as G
import HaskellShell.State

runList :: ShellState -> G.List -> IO ()
runList st = mapM_ (runPipeline st (stdin, False))

runPipeline :: ShellState -> (Handle, Bool) -> [G.PipelineElement] -> IO ()
runPipeline _  (input, True) [] = closeHandle input
runPipeline _  _             [] = return ()
runPipeline st (input, close) ((cmd, rs):rem) = do
  (mp, mhs) <- withStreams defaultStreams rs $ runCommand st cmd
  M.when close (closeHandle input)
  next <- runPipeline st
    (case listToMaybe (catMaybes mhs) of
      Just h -> (h, True)
      Nothing -> (stdin, False))
    rem
  case mp of
    Just pid -> M.void $ getProcessStatus True False pid
    Nothing  -> return ()
  where defaultStreams = [ (stdInput, input)
                         , (stdOutput, stdout)
                         , (stdError, stderr)
                         ]
        withStreams :: [(Fd, Handle)] -> [G.Redirection]
          -> ([(Fd, Handle)] -> IO a) -> IO (a, [Maybe Handle])
        withStreams hs [] f = do
          r <- f hs
          return (r, [Nothing])
        withStreams hs ((fds, dest):rs) f = case dest of
            G.File       path -> withBinaryFile path WriteMode  . flip curry Nothing
            G.AppendFile path -> withBinaryFile path AppendMode . flip curry Nothing
            G.Pipe            -> withPipe
          $ \(out, h) -> do
            (r, os) <- withStreams (insertForKeys fds h hs) rs f
            return (r, out:os)
        withPipe :: ((Maybe Handle, Handle) -> IO a) -> IO a
        withPipe = bracket (do
          (pr, pw) <- createPipe
          prh <- fdToHandle pr
          pwh <- fdToHandle pw
          return (Just prh, pwh))
          (closeHandle . snd)
        insertForKeys :: Eq k => [k] -> a -> [(k, a)] -> [(k, a)]
        insertForKeys ks a m = [(k, a) | k <- ks] ++ filter ((`notElem` ks) . fst) m

closeHandle :: Handle -> IO ()
closeHandle = handleToFd M.>=> closeFd

runCommand :: ShellState -> G.Command -> [(Fd, Handle)] -> IO (Maybe ProcessID)
runCommand _ [] _  = return Nothing
runCommand st cmd fds = case lookup (head cmd) builtins of
  Just builtin -> do
    runBuiltin st (lookup stdOutput fds) builtin cmd
    return Nothing
  Nothing -> do
    pid <- forkProcess $ do
      mapM_ (\(i, h) -> handleToFd h >>= moveTo i) fds
      f <- dup stdOutput; setFdOption f CloseOnExec True
      executeFile (head cmd) True (tail cmd) Nothing
        `Control.Exception.catch` execError f
      exitWith (ExitFailure 127)
    return (Just pid)
  where moveTo :: Fd -> Fd -> IO ()
        moveTo new old = M.when (old /= new) $ do
          dupTo old new; setFdOption old CloseOnExec True
        execError :: Fd -> IOException -> IO ()
        execError f e = do
          h <- fdToHandle f
          shellError h [head cmd, "command not found"]
          hFlush h

