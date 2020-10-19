{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gargoyle.PostgreSQL.Nix where

import Data.ByteString (ByteString)

import Gargoyle
import Gargoyle.PostgreSQL

import Paths_gargoyle_postgresql_nix
import System.Which

pgCtlPath :: FilePath
pgCtlPath = $(staticWhich "pg_ctl")

postgresNix :: IO (Gargoyle ByteString)
postgresNix = do
  bindir <- getBinDir
  return $ (mkPostgresGargoyle pgCtlPath)
    { _gargoyle_exec = bindir <> "/gargoyle-nix-postgres-monitor"
    }

postgresNixMonitor :: GargoyleMonitor FilePath
postgresNixMonitor = mkPostgresGargoyleMonitor pgCtlPath shutdownPostgresFast
