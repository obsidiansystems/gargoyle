{-# LANGUAGE TemplateHaskell #-}
module Main where

import Gargoyle
import Gargoyle.PostgreSQL
-- import Gargoyle.PostgreSQL.Nix
import System.Which


main :: IO ()
-- main = gargoyleMain postgresNixMonitor
main = gargoyleMain (mkPostgresGargoyleMonitor pgCtlPath shutdownPostgresFast)
  where
    pgCtlPath = $(staticWhich "pg_ctl")
