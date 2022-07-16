{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Gargoyle.PostgreSQL.Connect where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Gargoyle (withGargoyle)
import Gargoyle.PostgreSQL.Nix (postgresNix)
import System.Directory (doesFileExist)
import Data.Default.Class
import GHC.Generics
import Data.Time.Clock (NominalDiffTime)
import Data.Aeson.TH
import Control.Monad (guard)
import Data.List (uncons)
import Data.Maybe (fromMaybe)

-- | see $Data.Pool.createPool$
data PoolConfig = PoolConfig
  { _poolConfig_stripeCount :: !Int
  , _poolConfig_cacheTTL :: !NominalDiffTime
  , _poolConfig_maxResources :: !Int
  } deriving (Eq, Ord, Show, Generic)

instance Default PoolConfig where
  def = PoolConfig 1 5 20

-- | Like 'withDb'' but turns the connection string into a connection
-- 'Pool' for you and using 'error' on failure.
withDb :: String -> (Pool Connection -> IO a) -> IO a
withDb dbPath f = withDb' dbPath (openDb >=> f)

-- | Convert a connection string into a connection 'Pool'.
openDbWithConfig :: PoolConfig -> ByteString -> IO (Pool Connection)
openDbWithConfig cfg dbUri = createPool (connectPostgreSQL dbUri) close
  (_poolConfig_stripeCount cfg)
  (_poolConfig_cacheTTL cfg)
  (_poolConfig_maxResources cfg)

-- | Convert a connection string into a connection 'Pool'.
openDb :: ByteString -> IO (Pool Connection)
openDb = openDbWithConfig def

-- | Connects to a database using information at the given filepath.
-- The given filepath can be either a folder (for a local db)
-- or a file with a database connection string.
--
-- 'withDb'' takes a String, which represents the path to a database, and a
-- function that returns database connection information as arguments in
-- order to open and start the database. Otherwise, it will create the
-- database for you if it doesn't exist.
withDb' :: FilePath -> (ByteString -> IO a) -> IO a
withDb' dbPath f = do
  dbExists <- doesFileExist dbPath
  if dbExists
    -- use the file contents as the URI for an existing server
    then C8.readFile dbPath >>= f
    -- otherwise assume it's a folder for a local database
    else do
      g <- postgresNix
      withGargoyle g dbPath f

fmap concat $ traverse (deriveJSON (defaultOptions
  { fieldLabelModifier = \fieldLable -> fromMaybe fieldLable $ do
    (x,xs) <- uncons fieldLable
    guard (x == '_')
    (y, ys) <- uncons $ dropWhile (/= '_') xs
    guard (y == '_')
    guard $ not $ null ys
    pure ys
  , constructorTagModifier  = \constructorTag -> fromMaybe constructorTag $ do
    (y, ys) <- uncons $ dropWhile (/= '_') constructorTag
    guard (y == '_')
    guard $ not $ null ys
    pure ys
  , omitNothingFields       = False
  }))
    [ 'PoolConfig
    ]
