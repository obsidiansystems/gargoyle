module Main where

import System.Directory
import System.Exit
import System.FilePath
import System.Posix.Temp

import Gargoyle
import Gargoyle.PostgreSQL

main :: IO ()
main = do
  let testPostgres = defaultPostgres
        { _gargoyle_exec = "dist/build" </> "gargoyle-postgres-monitor/gargoyle-postgres-monitor"
        }
  --TODO make this exception safe
  testPath <- mkdtemp "psql-test"
  psqlLocal testPostgres "psql" (testPath </> "db") (Just "")
  removeDirectoryRecursive testPath
  exitSuccess
