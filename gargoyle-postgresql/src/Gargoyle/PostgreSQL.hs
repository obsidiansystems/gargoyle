{-# LANGUAGE OverloadedStrings #-}
module Gargoyle.PostgreSQL where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Search as BS
import Data.Foldable (for_)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory
import System.Exit
import System.IO
import System.Posix.Escape
import System.Posix.Signals
import System.Process

import Gargoyle

-- | A 'Gargoyle' that assumes `pg_ctl` is in the path and
-- will perform a 'fast shutdown' on termination (see below).
defaultPostgres :: Gargoyle ByteString
defaultPostgres = mkPostgresGargoyle "pg_ctl"

-- | A 'GargoyleMonitor' that assumes `pg_ctl` is on the path
defaultPostgresMonitor :: GargoyleMonitor FilePath
defaultPostgresMonitor = mkPostgresGargoyleMonitor "pg_ctl" shutdownPostgresFast

-- | Create a gargoyle by telling it where the relevant PostgreSQL executables are and
-- what it should do in order to shut down the server. This module provides two options:
-- 'shutdownPostgresSmart' and 'shutdownPostgresFast'.
mkPostgresGargoyle :: FilePath -- ^ Path to `pg_ctl`
                   -> Gargoyle ByteString
                   -- ^ The 'Gargoyle' returned provides to client code the connection
                   -- string that can be used to connect to the PostgreSQL server
mkPostgresGargoyle pgCtlPath = Gargoyle
  { _gargoyle_exec = "gargoyle-postgres-monitor"
  , _gargoyle_init = initLocalPostgres pgCtlPath
  , _gargoyle_getInfo = getLocalPostgresConnectionString
  }

mkPostgresGargoyleMonitor
  :: FilePath -- ^ Path to `pg_ctl`
  -> (FilePath -> FilePath -> IO ()) -- ^ Shutdown function
  -> GargoyleMonitor FilePath
mkPostgresGargoyleMonitor pgCtlPath shutdownFun = GargoyleMonitor
  { _gargoyleMonitor_start = startLocalPostgres pgCtlPath
  , _gargoyleMonitor_stop = shutdownFun pgCtlPath
  }

-- | Create a new PostgreSQL database in a local folder. This is a low level function used to
-- define the PostgreSQL 'Gargoyle'.
initLocalPostgres :: FilePath -- ^ Path to PostgreSQL `pg_ctl` executable
                  -> FilePath -- ^ Path in which to initialize PostgreSQL Server
                  -> IO ()
initLocalPostgres binPath dbDir = do
  devNull <- openFile "/dev/null" WriteMode
  (_, _, _, initdb) <- createProcess (proc binPath
    [ "init"
    , "-D", dbDir
    , "-o", escapeMany
      [ "-U", "postgres"
      , "--no-locale"
      , "-E", "UTF8"
      ]
    ]) { std_in = NoStream, std_out = UseHandle devNull, std_err = Inherit }
  r <- waitForProcess initdb
  case r of
    ExitSuccess -> return ()
    _ -> do
      putStrLn $ "initLocalPostgres failed: " ++ show r
      exitWith r

-- | Produces the connection string for a local postgresql database. This is a low level function
-- used to define the PostgreSQL 'Gargoyle'
getLocalPostgresConnectionString :: FilePath -> IO ByteString
getLocalPostgresConnectionString dbDir = do
  absoluteDbDir <- makeAbsolute dbDir
  return $ mconcat $
    [ "postgresql://postgres@"
    , (LBS.toStrict $ BS.replace "/" ("%2F" :: LBS.ByteString) $ T.encodeUtf8 $ T.pack absoluteDbDir)
    , "/postgres"
    ]

-- | Start a postgres server that is assumed to be in the given folder. This is a low level function
-- used to define the PostgreSQL 'Gargoyle'
startLocalPostgres :: FilePath -- ^ Path to PostgreSQL `pg_ctl` executable
                   -> FilePath -- ^ Path where the server to start is located
                   -> IO FilePath -- ^ handle of the PostgreSQL server
startLocalPostgres binPath dbDir = do
  absoluteDbDir <- makeAbsolute dbDir
  devNull <- openFile "/dev/null" WriteMode
  (_, _, _, postgres) <- createProcess (proc binPath
    [ "start"
    , "-D", absoluteDbDir
    , "-w"
    , "-o", escapeMany
      [ "-h", ""
      , "-k", absoluteDbDir
      ]
    ]) { std_in = NoStream, std_out = UseHandle devNull, std_err = Inherit }
  r <- waitForProcess postgres
  case r of
    ExitSuccess -> return absoluteDbDir
    _ -> do
      putStrLn $ "startLocalPostgres failed: " <> show r
      exitWith r

-- | Perform a "Smart Shutdown" of Postgres;
-- see http://www.postgresql.org/docs/current/static/server-shutdown.html
shutdownPostgresSmart :: FilePath -- ^ Path to PostgreSQL `pg_ctl` executable
                      -> FilePath -- ^ Path where the server to start is located
                      -> IO ()
shutdownPostgresSmart = shutdownPostgresWithMode "smart"

-- | Perform a "Fast Shutdown" of Postgres;
-- see http://www.postgresql.org/docs/current/static/server-shutdown.html
shutdownPostgresFast :: FilePath -- ^ Path to PostgreSQL `pg_ctl` executable
                      -> FilePath -- ^ Path where the server to start is located
                      -> IO ()
shutdownPostgresFast = shutdownPostgresWithMode "fast"

-- | Perform a "Immediate Shutdown" of Postgres;
-- see http://www.postgresql.org/docs/current/static/server-shutdown.html
shutdownPostgresImmediate :: FilePath -- ^ Path to PostgreSQL `pg_ctl` executable
                      -> FilePath -- ^ Path where the server to start is located
                      -> IO ()
shutdownPostgresImmediate = shutdownPostgresWithMode "immediate"

shutdownPostgresWithMode :: String -- ^ The shutdown mode to execute; see https://www.postgresql.org/docs/9.5/app-pg-ctl.html
                         -> FilePath -- ^ Path to PostgreSQL `pg_ctl` executable
                         -> FilePath -- ^ Path where the server to start is located
                         -> IO ()
shutdownPostgresWithMode mode binPath absoluteDbDir = do
  (_, _, _, postgres) <- createProcess (proc binPath
    [ "stop"
    , "-D", absoluteDbDir
    , "-w"
    , "-m", mode
    ]) { std_in = NoStream, std_out = NoStream, std_err = Inherit }
  r <- waitForProcess postgres
  case r of
    ExitSuccess -> return ()
    _ -> do
      putStrLn $ "stopLocalPostgres failed: " <> show r
      exitWith r

-- | Run `psql` against a Gargoyle managed db.
psqlLocal :: Gargoyle ByteString -- ^ 'Gargoyle' against which to run
          -> FilePath -- ^ The path to `psql`
          -> FilePath -- ^ The path where the managed daemon is expected
          -> Maybe String
          -- ^ Optionally provide stdin input instead of an inheriting current stdin.
          -- It will have a newline and quit command appended to it.
          -> IO ()
psqlLocal g psqlPath dbPath minput = withGargoyle g dbPath $ \dbUri -> do
  void $ installHandler keyboardSignal Ignore Nothing
  let psqlProc = (proc psqlPath [ T.unpack $ T.decodeUtf8 dbUri ])
        { std_in = case minput of
            Nothing -> Inherit
            Just _ -> CreatePipe
        , std_out = Inherit
        , std_err = Inherit
        }
  (mStdin, _, _, psql) <- createProcess psqlProc
  case minput of
    Nothing -> return ()
    Just input -> hPutStrLn (fromJust mStdin) (input ++ "\n\\q")
  ExitSuccess <- waitForProcess psql
  return ()

-- | Run an arbitrary process against a Gargoyle-managed DB, providing connection
--   information by substituting a given argument pattern with the connection string.
runPgLocalWithSubstitution
  :: Gargoyle ByteString -- ^ 'Gargoyle' against which to run
  -> FilePath -- ^ The path where the managed daemon is expected
  -> FilePath -- ^ Path to process to run
  -> (String -> [String]) -- ^ Function producing arguments to the process given the connection string
  -> Maybe String -- ^ Optionally provide stdin input instead of an inheriting current stdin.
  -> IO ExitCode
runPgLocalWithSubstitution g dbPath procPath mkProcArgs mInput = withGargoyle g dbPath $ \dbUri -> do
  void $ installHandler keyboardSignal Ignore Nothing
  let
    procSpec = (proc procPath $ mkProcArgs $ T.unpack $ T.decodeUtf8 dbUri)
      { std_in = case mInput of
          Nothing -> Inherit
          Just _ -> CreatePipe
      , std_out = Inherit
      , std_err = Inherit
      }
  withCreateProcess procSpec $ \mStdin _ _ procHandle -> do
    for_ mInput $
      hPutStrLn (fromMaybe (error "runPgLocalWithSubstitution: input stream was expected") mStdin)
    waitForProcess procHandle
