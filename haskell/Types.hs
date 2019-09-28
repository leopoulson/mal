module Types where

data MVal =
  MNum Int |
  MStr String |
  MSym String |
  MList [MVal] |
  MFun ([MVal] -> MVal) |
  MErr String |
  MNil |
  MTrue |
  MFalse

instance Eq MVal where
  MNum n == MNum m = n == m
  MStr st == MStr st' = st == st'
  MSym ch == MSym ch' = ch == ch'
  MList ms == MList ms' = ms == ms'
  MErr e == MErr e' = e == e'
  MNil == MNil = True
  MTrue == MTrue = True
  MFalse == MFalse = False
  _ == _ = False

instance Show MVal where
  show (MNum n) = "MNum " ++ show n
  show (MStr str) = "MStr " ++ show str
  show (MSym ch) = "MSym " ++ show ch
  show (MList ms) = "MList " ++ show ms
  show (MFun _) = "MFun"
  show (MErr err) = "MErr " ++ show err
  show MNil = "nil"
  show MTrue = "true"
  show MFalse = "false"

instance Ord MVal where
  compare (MNum a) (MNum b) = compare a b
  compare (MStr a) (MStr b) = compare a b
  compare (MSym a) (MSym b) = compare a b
  compare (MList a) (MList b) = compare a b
  compare (MErr a) (MErr b) = compare a b
  compare (MFun _) (MFun _) = EQ
  compare (MNum _) _ = EQ --why
  compare MNil MNil = EQ
  compare MTrue MTrue = EQ
  compare MFalse MFalse = EQ
  compare _ _ = EQ
