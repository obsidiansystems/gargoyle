{-# LANGUAGE TemplateHaskell #-}
module Main where

import Gargoyle
import Gargoyle.PostgreSQL
import System.Which

main :: IO ()
main = gargoyleMain (mkPostgresGargoyleMonitor pgCtlPath shutdownPostgresFast)
  where
    pgCtlPath = $(staticWhich "pg_ctl")
