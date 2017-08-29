{-# LANGUAGE OverloadedStrings #-}
module Gargoyle.PostgreSQL where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Search as BS
import Data.Function
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory
import System.Exit
import System.IO
import System.Posix.Signals
import System.Process
import System.Process.Internals

import Gargoyle

-- | A 'Gargoyle' that assumes `initdb` and `postgres` are in the path and
-- will perform a 'fast shutdown' on termination (see below).
defaultPostgres :: Gargoyle ProcessHandle ByteString
defaultPostgres = mkPostgresGargoyle "initdb" "postgres" shutdownPostgresFast

-- | Create a gargoyle by telling it where the relevant PostgreSQL executables are and
-- what it should do in order to shut down the server. This module provides two options:
-- 'shutdownPostgresSmart' and 'shutdownPostgresFast'.
mkPostgresGargoyle :: FilePath -- ^ Path to `initdb`
                   -> FilePath -- ^ Path to `postgres`
                   -> (ProcessHandle -> IO ()) -- ^ Shutdown function
                   -> Gargoyle ProcessHandle ByteString
                   -- ^ The 'Gargoyle' returned provides to client code the connection
                   -- string that can be used to connect to the PostgreSQL server
mkPostgresGargoyle initdbPath postgresPath shutdownFun = Gargoyle
  { _gargoyle_exec = "gargoyle-postgres-monitor"
  , _gargoyle_init = initLocalPostgres initdbPath
  , _gargoyle_start = startLocalPostgres postgresPath
  , _gargoyle_stop = shutdownFun
  , _gargoyle_getInfo = getLocalPostgresConnectionString
  }

-- | Create a new PostgreSQL database in a local folder. This is a low level function used to
-- define the PostgreSQL 'Gargoyle'.
initLocalPostgres :: FilePath -- ^ Path to PostgreSQL `initdb` executable
                  -> FilePath -- ^ Path in which to initialize PostgreSQL Server
                  -> IO ()
initLocalPostgres binPath dbDir = do
  (_, _, _, initdb) <- runInteractiveProcess binPath
    [ "-D", dbDir
    , "-U", "postgres"
    , "--no-locale"
    , "-E", "UTF8"
    ] Nothing Nothing
  ExitSuccess <- waitForProcess initdb
  return ()

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
startLocalPostgres :: FilePath -- ^ Path to PostgreSQL `postgres` executable
                   -> FilePath -- ^ Path where the server to start is located
                   -> IO ProcessHandle -- ^ handle of the PostgreSQL server
startLocalPostgres binPath dbDir = do
  absoluteDbDir <- makeAbsolute dbDir
  (_, _, err, postgres) <- runInteractiveProcess binPath
    [ "-h", ""
    , "-D", absoluteDbDir
    , "-k", absoluteDbDir
    ] Nothing Nothing
  fix $ \loop -> do
    l <- hGetLine err
    let (tag, rest) = span (/= ':') l
    when (tag /= "LOG") $ fail $ "startLocalPostgres: Unexpected output from postgres: " <> show l
    when (rest /= ":  database system is ready to accept connections") loop
  return postgres

-- | Perform a "Smart Shutdown" of Postgres;
-- see http://www.postgresql.org/docs/current/static/server-shutdown.html
shutdownPostgresSmart :: ProcessHandle -- ^ handle of the PostgreSQL server
                      -> IO ()
shutdownPostgresSmart postgres = do
  terminateProcess postgres
  _ <- waitForProcess postgres
  return ()

-- | Perform a "Fast Shutdown" of Postgres;
-- see http://www.postgresql.org/docs/current/static/server-shutdown.html
shutdownPostgresFast :: ProcessHandle -- ^ handle of the PostgreSQL server
                     -> IO ()
shutdownPostgresFast postgres = do
  withProcessHandle postgres $ \p -> do
    case p of
      ClosedHandle _ -> return ()
      OpenHandle h -> signalProcess sigINT h
  _ <- waitForProcess postgres
  return ()

-- | Run `psql` against a Gargoyle managed db.
psqlLocal :: Gargoyle pid ByteString -- ^ 'Gargoyle' against which to run
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
