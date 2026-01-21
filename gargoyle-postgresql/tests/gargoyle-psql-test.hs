{-# LANGUAGE NumDecimals #-}
module Main where

import Control.Concurrent
import System.Directory
import System.Exit
import System.FilePath
import System.Posix.Temp

import Gargoyle
import Gargoyle.PostgreSQL

main :: IO ()
main = do
  let testPostgres = defaultPostgres
        { _gargoyle_exec = "gargoyle-postgres-monitor"
        }
  --TODO make this exception safe
  testPath <- mkdtemp "psql-test"
  psqlLocal testPostgres "psql" (testPath </> "db") (Just "")
  threadDelay 1e6 --TODO: workaround for apparent race condition when removing db/work
  removeDirectoryRecursive testPath
  exitSuccess
