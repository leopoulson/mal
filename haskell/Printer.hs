module Printer where

import Types

prStr :: MVal -> String
prStr (MNum n) = show n
prStr (MStr str) = show str
prStr (MSym ch) = show ch
prStr (MList ms) = "(" ++ unwords (map prStr ms) ++ ")"
