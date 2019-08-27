{-# LANGUAGE LambdaCase #-}
module Gargoyle.PostgreSQL.Connect
  ( openDb
  , withDb
  , withDbConnString
  ) where

import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Gargoyle (withGargoyle)
import Gargoyle.PostgreSQL.Nix (postgresNix)
import System.Directory (doesFileExist)

-- | Connects to a database using information at the given filepath
-- The given filepath can be either a folder (for a local db)
-- or a file with a database url
--
-- withDb takes a String, which represents the path to a database, and a
-- function that returns database connection information as arguments in
-- order to open and start the database. Otherwise, it will create the
-- database for you if it doesn't exist.
withDb :: String -> (Pool Connection -> IO a) -> IO a
withDb dbPath k = do
  dbExists <- doesFileExist dbPath
  flip withDbConnString k =<< if dbExists
    -- use the file contents as the uri for an existing server
    then Left . head . C8.lines <$> C8.readFile dbPath
    -- otherwise assume it's a folder for a local database
    else pure $ Right dbPath

-- | Either connects to a database at the given connection string in the Left
-- case, or uses gargoyle at the filepath specified in the Right case.  Allows
-- to keep the connection string at a different place from the gargoyle
-- cluster.
withDbConnString :: Either ByteString FilePath -> (Pool Connection -> IO a) -> IO a
withDbConnString = \case
  -- use the file contents as the uri for an existing server
  Left connStr -> (>>=) (openDb connStr)
  -- otherwise assume it's a folder for a local database
  Right gargoylePath -> \k -> do
    g <- postgresNix
    withGargoyle g gargoylePath $ k <=< openDb

openDb :: ByteString -> IO (Pool Connection)
openDb dbUri = createPool (connectPostgreSQL dbUri) close 1 5 20
