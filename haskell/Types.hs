module Types where

data MVal =
  MNum Int |
  MStr String |
  MSym Char |
  MList [MVal] |
  MFun ([MVal] -> MVal)
 -- deriving (Eq)

instance Eq MVal where
  MNum n == MNum m = n == m
  MStr st == MStr st' = st == st'
  MSym ch == MSym ch' = ch == ch'
  MList ms == MList ms' = ms == ms'
  _ == _ = False

instance Show MVal where
  show (MNum n) = "MNum " ++ show n
  show (MStr str) = "MStr " ++ show str
  show (MSym ch) = "MSym " ++ show ch
  show (MList ms) = "MList " ++ show ms
  show (MFun fn) = "MFun "
