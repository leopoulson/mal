module Env
  (Env (..), set, find, get, newEnv)
where


import Types

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Debug.Trace as D

type Key = MVal
type Value = MVal

data Env = Env {inner :: Map.Map Key Value, outer :: Maybe Env}
  deriving (Eq, Show)

set :: Key -> Value -> Env -> Env
set k v@(MStr s) env
  | lastN 10 s == "not found." = trace "nf" env
  | otherwise                  = trace "ok" Env (Map.insert k v $ inner env) (outer env)
set k (MErr err) env = trace err env
set k v env = trace (show v) Env (Map.insert k v $ inner env) (outer env)

-- (def! x abc)
find :: Key -> Env -> Maybe Env
find k env = if Map.member k (inner env)
             then Just env
             else outer env >>= find k

get :: Key -> Env -> Maybe Value
get k env = find k env >>= Map.lookup k . inner

newEnv :: Env
newEnv = Env Map.empty Nothing


-- utility
lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs (drop n xs)
