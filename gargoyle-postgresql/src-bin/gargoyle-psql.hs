{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import System.Environment
import System.Exit

import Gargoyle.PostgreSQL

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dbPath] -> psqlLocal defaultPostgres "psql" dbPath Nothing
    _ -> do
      pname <- getProgName
      putStrLn $ intercalate " "
        [ "USAGE:", pname, "<path>" ]
      putStrLn "\t<path>: path to local db"
      exitFailure
  return ()
