{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gargoyle.PostgreSQL.Nix where

import Data.ByteString (ByteString)
import Data.Monoid
import System.Process

import Gargoyle
import Gargoyle.PostgreSQL

import Paths_gargoyle_postgresql_nix
import System.Which

postgresNix :: IO (Gargoyle ProcessHandle ByteString)
postgresNix = do
  bindir <- getBinDir
  return $ (mkPostgresGargoyle $(staticWhich "initdb") $(staticWhich "postgres") shutdownPostgresFast)
    { _gargoyle_exec = bindir <> "/gargoyle-nix-postgres-monitor"
    }
