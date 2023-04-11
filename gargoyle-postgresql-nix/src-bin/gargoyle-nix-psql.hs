{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List
import Data.List.Split
import System.Environment
import System.Exit

import Gargoyle.PostgreSQL
import Gargoyle.PostgreSQL.Nix
import System.Which

monitorExe :: FilePath
monitorExe = $(staticWhich "gargoyle-nix-postgres-monitor")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dbPath] -> do
      let exe = splitOn "/" monitorExe
          path = intercalate "/" (init exe)
      setEnv "gargoyle_postgresql_nix_bindir" path
      print path
      g <- postgresNix monitorExe
      psqlLocal g $(staticWhich "psql") dbPath Nothing
    _ -> do
      pname <- getProgName
      putStrLn $ intercalate " "
        [ "USAGE:", pname, "<path>" ]
      putStrLn "\t<path>: path to local db"
      exitFailure
  return ()
