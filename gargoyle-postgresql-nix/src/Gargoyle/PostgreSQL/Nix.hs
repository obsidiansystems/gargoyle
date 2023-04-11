{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gargoyle.PostgreSQL.Nix where

import Data.ByteString (ByteString)

import Gargoyle
import Gargoyle.PostgreSQL

import System.Which

monitorExe :: FilePath
monitorExe = $(staticWhich "gargoyle-nix-postgres-monitor")

postgresNix :: IO (Gargoyle FilePath ByteString)
postgresNix = do
  return $ (mkPostgresGargoyle $(staticWhich "pg_ctl") shutdownPostgresFast)
    { _gargoyle_exec = monitorExe
    }
