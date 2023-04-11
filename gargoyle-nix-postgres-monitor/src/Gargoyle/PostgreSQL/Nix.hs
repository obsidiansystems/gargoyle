{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Gargoyle.PostgreSQL.Nix where

import Data.ByteString (ByteString)

import Gargoyle
import Gargoyle.PostgreSQL

import System.Which

postgresNix :: FilePath -> IO (Gargoyle FilePath ByteString)
postgresNix bindir = do
  return $ (mkPostgresGargoyle $(staticWhich "pg_ctl") shutdownPostgresFast)
    { _gargoyle_exec = bindir
    }
