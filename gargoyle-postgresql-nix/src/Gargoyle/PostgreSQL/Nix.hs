{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gargoyle.PostgreSQL.Nix where

import Data.ByteString (ByteString)

import Gargoyle
import Gargoyle.PostgreSQL

import System.Environment(setEnv)
import Data.List.Split
import Data.List

import System.Which

monitorExe :: FilePath
monitorExe = $(staticWhich "gargoyle-nix-postgres-monitor")

postgresNix :: IO (Gargoyle FilePath ByteString)
postgresNix = do
  let exe = splitOn "/" monitorExe
      path = intercalate "/" (init exe)
  setEnv "gargoyle_postgresql_nix_bindir" path
  return $ (mkPostgresGargoyle $(staticWhich "pg_ctl") shutdownPostgresFast)
    { _gargoyle_exec = monitorExe
    }
