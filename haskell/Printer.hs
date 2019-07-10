module Printer where

import Types

prStr :: MVal -> String
prStr (MNum n) = show n
prStr (MStr str) = str
prStr (MSym ch) = [ch]
prStr (MList ms) = "(" ++ unwords (map prStr ms) ++ ")"
