module Main where

import Gargoyle
import Gargoyle.PostgreSQL.Nix

--import Paths_gargoyle_postgresql_nix

placeholder :: FilePath
placeholder = ""

main :: IO ()
main = do
    (postgresNix placeholder) >>= gargoyleMain
