module Main where

import Gargoyle
import Gargoyle.PostgreSQL.Nix

main :: IO ()
main = postgresNix >>= gargoyleMain
