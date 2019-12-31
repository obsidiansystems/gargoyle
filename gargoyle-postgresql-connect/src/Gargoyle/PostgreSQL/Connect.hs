module Gargoyle.PostgreSQL.Connect (withDb, withDb', openDb) where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Gargoyle (withGargoyle)
import Gargoyle.PostgreSQL.Nix (postgresNix)
import System.Directory (doesFileExist)


-- Like 'withDb'' but turns the connection string into a connection 'Pool' for you.
withDb :: String -> (Pool Connection -> IO a) -> IO a
withDb dbPath f = withDb' dbPath (openDb >=> f)

-- Convert a connection string into a connection 'Pool'.
openDb :: ByteString -> IO (Pool Connection)
openDb dbUri = createPool (connectPostgreSQL dbUri) close 1 5 20

-- | Connects to a database using information at the given filepath.
-- The given filepath can be either a folder (for a local db)
-- or a file with a database connection string.
--
-- 'withDb'' takes a String, which represents the path to a database, and a
-- function that returns database connection information as arguments in
-- order to open and start the database. Otherwise, it will create the
-- database for you if it doesn't exist.
withDb' :: String -> (ByteString -> IO a) -> IO a
withDb' dbPath f = do
  dbExists <- doesFileExist dbPath
  if dbExists
    -- use the file contents as the URI for an existing server
    then C8.readFile dbPath >>= \b -> case C8.lines b of
      [] -> error "DB connection string configuration file is empty"
      -- TODO: Consider also blowing up if more than one line for next breaking release
      x:_ -> f x
    -- otherwise assume it's a folder for a local database
    else do
      g <- postgresNix
      withGargoyle g dbPath f
