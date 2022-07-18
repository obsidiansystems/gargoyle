{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Gargoyle.PostgreSQL.Connect where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Gargoyle (withGargoyle, Gargoyle(..))
import Gargoyle.PostgreSQL.Nix (postgresNix)
import System.Directory (doesFileExist)
import Data.Default.Class
import GHC.Generics
import Data.Time.Clock (NominalDiffTime)
import Data.Aeson.TH
import Control.Monad (guard)
import Data.List (uncons)
import Data.Maybe (fromMaybe)

-- | configuration options for a resource pool.
--
-- The exact fields of this type are subject to change; consider using
-- the $Default$ instance when needed, and adjust as desired.
--
-- see $Data.Pool.createPool$ for explanation of what the fields mean.
data PoolConfig = PoolConfig
  { _poolConfig_stripeCount :: !Int
  , _poolConfig_cacheTTL :: !NominalDiffTime
  , _poolConfig_maxResources :: !Int
  } deriving (Eq, Ord, Show, Generic)

instance Default PoolConfig where
  def = PoolConfig 1 5 20

-- | configuration options for gargoyle managed postgres instances.
--
-- The exact fields of this type are subject to change; consider using
-- $mkGargoylePostgresOptions$, $gargoylePostgresOptions$,
-- or the $Default$ instance when needed, and adjust as desired.
data GargoylePostgresOptions = GargoylePostgresOptions
  { _gargoylePostgresOptions_dbPath :: FilePath -- ^ path to gargoyle workDir.
  , _gargoylePostgresOptions_gargoyleInit :: FilePath -> IO () -- ^ effect to perform on postgres database at first setup.
  }

instance Default GargoylePostgresOptions where
  def = mkGargoylePostgresOptions "db"

-- | a helper functon to get a baseline $GargoylePostgresOptions$ at a
-- particular path.
mkGargoylePostgresOptions :: FilePath -> GargoylePostgresOptions
mkGargoylePostgresOptions dbPath = GargoylePostgresOptions
  dbPath
  (\_ -> pure ())

gargoylePostgresOptions :: Maybe ByteString -> FilePath -> Either ByteString GargoylePostgresOptions
gargoylePostgresOptions = \case
  Just b -> \_ -> Left b
  Nothing -> Right . mkGargoylePostgresOptions

-- | look at a filepath, if it's a file, interpret its contents as a connection
-- string, otherwise (if absent or a directory), interpret it as a path to a
-- gargoyle managed postgres db.
getGargoylePostgresOptions :: FilePath -> IO (Either ByteString GargoylePostgresOptions)
getGargoylePostgresOptions dbPath = do
  dbExists <- doesFileExist dbPath
  if dbExists
    -- use the file contents as the URI for an existing server
    then Left <$> C8.readFile dbPath
    -- otherwise assume it's a folder for a local database
    else pure (Right (mkGargoylePostgresOptions dbPath))

withDbPool
  :: PoolConfig
  -> Either ByteString GargoylePostgresOptions -- ^ connstring or path to gargoyle managed db
  -> (Pool Connection -> IO a) -- ^ callback which uses the configured pool.  The configured gargoyle instance is guaranteed only until this function returns.
  -> IO a
withDbPool poolOpts gargoyleOpts k = withDbConnString gargoyleOpts (openDbWithConfig poolOpts >=> k)

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
  gargoyleOpts <- getGargoylePostgresOptions dbPath
  withDbConnString gargoyleOpts f

withDbConnString
  :: Either ByteString GargoylePostgresOptions -- ^ connstring or path to gargoyle managed db
  -> (ByteString -> IO a) -- ^ callback which uses the resulting connection string.  The configured gargoyle-postgres instance is guaranteed only until this function returns.
  -> IO a
withDbConnString dbOptions k = case dbOptions of
  Left dbUri -> k dbUri
  Right (GargoylePostgresOptions dbPath gargoyleInit) -> do
    defaultConfig <- postgresNix
    let myConfig = defaultConfig
          { _gargoyle_init = _gargoyle_init defaultConfig <> gargoyleInit
          }
    withGargoyle myConfig dbPath k


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
