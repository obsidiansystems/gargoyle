{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List
import System.Environment
import System.Exit

import Gargoyle.PostgreSQL
import Gargoyle.PostgreSQL.Nix
import System.Which

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dbPath] -> do
      g <- postgresNix
      psqlLocal g $(staticWhich "psql") dbPath Nothing
    _ -> do
      pname <- getProgName
      putStrLn $ intercalate " "
        [ "USAGE:", pname, "<path>" ]
      putStrLn "\t<path>: path to local db"
      exitFailure
  return ()
