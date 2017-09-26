-- | Utilities for running a daemon in a local directory
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gargoyle
  ( Gargoyle (..)
  , withGargoyle
  , gargoyleMain
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Network.Socket
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Environment
import System.FileLock
import System.Process

import Debug.Trace

data Gargoyle pid a = Gargoyle
  { _gargoyle_exec :: FilePath
    -- ^ The path to the executable created with 'gargoyleMain' which will serve as the daemon
    -- monitor process.
  , _gargoyle_init :: FilePath -> IO ()
    -- ^ The action to run in order to populate the daemon's environment for the first run.
  , _gargoyle_start :: FilePath -> IO pid
    -- ^ The action to run in order to spin up the daemon on every run. This happens after
    -- '_gargoyle_init' if it also runs.
  , _gargoyle_stop :: pid -> IO ()
    -- ^ The action to run when the monitor process detects that no clients are connected anymore.
  , _gargoyle_getInfo :: FilePath -> IO a
    -- ^ Run a command which knows about the working directory of the daemon to collect runtime
    -- information to pass to client code in 'withGargoyle'.
  }

gControlDir :: FilePath -> FilePath
gControlDir = (</> "control")

gWorkDir :: FilePath -> FilePath
gWorkDir = (</> "work")

gOldWorkDir :: FilePath -> FilePath
gOldWorkDir = (</> "db")

gLockDir :: FilePath -> FilePath
gLockDir = (</> "lock")

checkThreadedRuntime :: IO ()
checkThreadedRuntime = when (not rtsSupportsBoundThreads) $ do
  hPutStrLn stderr "Gargoyle requires threaded run-time, aborting"
  assert rtsSupportsBoundThreads (return ()) -- throws an AssertionFailed exception

-- | Run an IO action while maintaining a connection to a daemon. The daemon will automatically be
-- stopped when no clients remain. If the daemon has not yet been initialized, it will be.
-- The counterpart of this function is 'gargoyleMain' which should be used to produce an executable
-- that will monitor the daemon's status.
withGargoyle :: Gargoyle pid a -- ^ Description of how to manage the daemon.
             -> FilePath -- ^ The directory where the daemon should be initialized.
             -> (a -> IO b)
                -- ^ Client action which has access to runtime information provided by
                -- the 'Gargoyle'.
             -> IO b
                -- ^ By the time this function returns, the monitor process is aware that the
                -- the client is no longer interested in the daemon.
withGargoyle g daemonDir b = do
  checkThreadedRuntime
  daemonExists <- doesDirectoryExist daemonDir
  if daemonExists
    then do
    let oldWrk = gOldWorkDir daemonDir
        wrk = gWorkDir daemonDir
    oldWorkDirExists <- doesDirectoryExist oldWrk
    workDirExists <- doesDirectoryExist wrk
    when (oldWorkDirExists && not workDirExists) $ renameDirectory oldWrk wrk
    else do
    createDirectory daemonDir
    _gargoyle_init g (gWorkDir daemonDir)
  s <- socket AF_UNIX Stream defaultProtocol
  let acquire = do
        connectResult <- try $ connect s $ SockAddrUnix $ gControlDir daemonDir
        case connectResult of
          Right () -> return ()
          Left e
            | isDoesNotExistError e -> do
              let monProc = (proc (_gargoyle_exec g) [daemonDir])
                    { std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = Inherit }
              (Just monIn, Just monOut, Nothing, monHnd) <- createProcess monProc
              void $ forkOS $ void $ waitForProcess monHnd
              hClose monIn
              r <- hGetLine monOut
              case r of
                "retry" -> do
                  threadDelay 500000 -- These are expensive ops so don't try too hard
                  acquire -- Try again
                "ready" -> acquire
                _ -> fail "Unexpected gargoyle message from monitor process"
            | otherwise -> throwIO e
  bracket_ acquire (shutdown s ShutdownBoth >> close s) $
    b =<< _gargoyle_getInfo g (gWorkDir daemonDir)

-- | Run a local daemon over a domain socket; the daemon will be automatically stopped when
-- no clients remain. This function assumes that the daemon has already been initialized
-- in the specified location. This function should be used as the main function of an executable
-- which will then be invoked by calling 'withGargoyle' in the client code to monitor
-- the daemon's status.
gargoyleMain :: Gargoyle pid a
             -- ^ Description of how to initialize, spin up, and spin down a daemon.
             -> IO () -- ^ Returns only when all clients have disconnected.
gargoyleMain g = do
  checkThreadedRuntime
  [daemonDir] <- getArgs >>= \case
    x@[_] -> return x
    _ -> fail "Gargoyle monitor received unexpected number of arguments"
  let lockPath = gLockDir daemonDir
  -- Make sure the lock file is there
  catch (openFile lockPath WriteMode >>= hClose) $ \(e :: IOException) -> if
    | isAlreadyInUseError e -> return ()
    | isDoesNotExistError e -> throwIO e -- this means it's not a file but it exists
    | isPermissionError e -> throwIO e -- the daemon directory is in a bad state
  -- The daemon tries to hold on to the lock file for its lifetime, signaling that it is
  -- accepting connections.
  lock <- tryLockFile lockPath Exclusive >>= \case
    Just x -> return x
    Nothing -> do
      putStrLn "retry"
      hFlush stdout
      exitFailure
  -- Clients must maintain a connection to controlSocket to ensure that
  -- the daemon doesn't get shut down
  controlSocket <- socket AF_UNIX Stream defaultProtocol
  let socketPath = gControlDir daemonDir
      createSocket = do
        result <- try $ bind controlSocket $ SockAddrUnix socketPath
        case result of
          Right () -> return ()
          Left e
            | isAlreadyInUseError e
            -> do
              -- This is safe because all gargoyle monitors try to take a lock before trying
              -- to mess with the control socket. This avoids race conditions provided that
              -- only gargoyle monitors touch the daemon directory.
              removePathForcibly socketPath
              putStrLn "retry"
              hFlush stdout
              exitFailure
            | otherwise -> throwIO e
  bracket createSocket (\_ -> removeFile socketPath) $ \_ -> do
    -- Between bind and listen, the socket will be in a non-accepting state;
    -- this should last a very brief time, so the client should just briefly wait and then retry
    listen controlSocket 128
    -- TODO: There is a failure mode here: if an interloper connects and disconnects before
    -- the initial caller connects, the initial caller will fail to connect; instead, we should
    -- start up with an existing connection (possibly a pipe passed in from the parent process) and
    -- with this var set to 1
    numClientsVar <- newMVar (0 :: Int)
    -- When this var is filled, the server will shut down
    shutdownVar <- newEmptyMVar
    void $ forkOS $ forever $ do
      (s, _) <- accept controlSocket
      --TODO: What happens if we decide we're shutting down here?
      modifyMVar_ numClientsVar $ \n -> do
        return $ succ n
      forkOS $ do
        h <- socketToHandle s ReadMode
        -- Block until we hit EOF; if we successfully read a character that means the client is
        -- in violation of the protocol, so we shut them down too
        catchJust
          (\e -> if isEOFError e then Just () else e `traceShow` Nothing)
          (hGetChar h >> hPutStrLn stderr "Warning: client sent data over the control socket")
          return
        mask_ $ do
          n <- takeMVar numClientsVar
          case pred n of
            0 -> do
              shutdown controlSocket ShutdownBoth
              putMVar shutdownVar ()
            n' -> putMVar numClientsVar n'
    bracket (_gargoyle_start g (gWorkDir daemonDir)) (_gargoyle_stop g) $ \_ -> do
      hSetBuffering stdout LineBuffering
      putStrLn "ready" -- Signal to the invoker that we're ready
      takeMVar shutdownVar
  unlockFile lock
