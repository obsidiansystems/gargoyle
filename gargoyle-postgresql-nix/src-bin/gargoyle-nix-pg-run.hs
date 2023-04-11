{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.Environment (getArgs, getProgName, setEnv)
import System.Exit (exitFailure, exitWith)
import System.Which
import Data.List
import Data.List.Split

import Gargoyle.PostgreSQL (runPgLocalWithSubstitution)
import Gargoyle.PostgreSQL.Nix (postgresNix)

monitorExe :: FilePath
monitorExe = $(staticWhich "gargoyle-nix-postgres-monitor")

main :: IO ()
main = do
  args <- getArgs
  case args of
    dbPath:cmd:cmdArgs -> do
      let exe = splitOn "/" monitorExe
          path = intercalate "/" (init exe)
      setEnv "gargoyle_postgresql_nix_bindir" path
      print path
      pg <- postgresNix monitorExe
      exitWith =<< runPgLocalWithSubstitution pg dbPath cmd (\conn -> if null cmdArgs then [conn] else substArgs conn cmdArgs) Nothing
    _ -> do
      pname <- getProgName
      putStrLn $ unwords [ "USAGE:", pname, "<path>", "<command>", "[...<arguments>]" ]
      putStrLn "\t<path>: path to local db"
      putStrLn "\t<command>: command to run"
      putStrLn "\t<arguments>: list of arguments to <command> where the special argument '{}' is expanded into a connection string; if <arguments> is empty, '{}' will be supplied as the only argument by default"
      exitFailure
  where
    substArgs conn = map (\x -> if x == "{}" then conn else x)
