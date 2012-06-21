module HaskellShell.Run (runList) where

import Control.Exception (bracket)
import qualified Control.Monad as M
import qualified Data.Map as Map
import Data.Maybe

import System.IO
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types (Fd, ProcessID)

import HaskellShell.Builtins
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
  where defaultStreams = Map.fromList [ (stdInput, input)
                                      , (stdOutput, stdout)
                                      , (stdError, stderr)
                                      ]
        withStreams :: Map.Map Fd Handle -> [G.Redirection]
          -> (Map.Map Fd Handle -> IO a) -> IO (a, [Maybe Handle])
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
        insertForKeys :: Ord k => [k] -> a -> Map.Map k a -> Map.Map k a
        insertForKeys [] _ m = m
        insertForKeys (k:ks) a m = insertForKeys ks a (Map.insert k a m)

closeHandle :: Handle -> IO ()
closeHandle h = handleToFd h >>= closeFd

runCommand :: ShellState -> G.Command -> Map.Map Fd Handle -> IO (Maybe ProcessID)
runCommand _ [] _  = return Nothing
runCommand st cmd fds = case lookup (head cmd) builtins of
  Just builtin -> do
    runBuiltin st (Map.lookup stdOutput fds) builtin cmd
    return Nothing
  Nothing -> do
    pid <- forkProcess $ do
      Map.traverseWithKey (\i h -> handleToFd h >>= flip dupTo i) fds
      executeFile (head cmd) True (tail cmd) Nothing
    return (Just pid)

