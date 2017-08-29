module Main where

import Gargoyle
import Gargoyle.PostgreSQL

main :: IO ()
main = gargoyleMain defaultPostgres
