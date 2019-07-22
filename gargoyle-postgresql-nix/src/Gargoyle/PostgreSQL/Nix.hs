{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gargoyle.PostgreSQL.Nix where

import Data.ByteString (ByteString)

import Gargoyle
import Gargoyle.PostgreSQL

import Paths_gargoyle_postgresql_nix
import System.Which

postgresNix :: IO (Gargoyle FilePath ByteString)
postgresNix = do
  bindir <- getBinDir
  return $ (mkPostgresGargoyle $(staticWhich "pg_ctl") shutdownPostgresFast)
    { _gargoyle_exec = bindir <> "/gargoyle-nix-postgres-monitor"
    }
