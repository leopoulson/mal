module Types where

data MVal =
  MNum Int |
  MStr String 
  deriving (Show, Eq)
