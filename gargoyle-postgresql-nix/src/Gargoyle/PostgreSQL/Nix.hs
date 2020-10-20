{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gargoyle.PostgreSQL.Nix where

import Data.ByteString (ByteString)

import Gargoyle
import Gargoyle.PostgreSQL

import System.Which

pgCtlPath :: FilePath
pgCtlPath = $(staticWhich "pg_ctl")

monitorPath :: FilePath
monitorPath = $(staticWhich "gargoyle-nix-postgres-monitor")

postgresNix :: IO (Gargoyle ByteString)
postgresNix = do
  return $ (mkPostgresGargoyle pgCtlPath)
    { _gargoyle_exec = monitorPath
    }

postgresNixMonitor :: GargoyleMonitor FilePath
postgresNixMonitor = mkPostgresGargoyleMonitor pgCtlPath shutdownPostgresFast
