module Types where

data MVal =
  MNum Int |
  MStr String |
  MSym Char |
  MList [MVal]
  deriving (Eq, Show)

