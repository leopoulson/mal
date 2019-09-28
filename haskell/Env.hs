module Env
  (Env (..), set, find, get, newEnv, outerC, doBinds)
where


import Types

import qualified Data.Map as Map

-- import Debug.Trace as D

type Key = MVal
type Value = MVal

data Env = Env {inner :: Map.Map Key Value, outer :: Maybe Env}
  deriving (Eq, Show)

set :: Key -> Value -> Env -> Env
set k v env = Env (Map.insert k v $ inner env) (outer env)

find :: Key -> Env -> Maybe Env
find k env = if Map.member k (inner env)
             then Just env
             else outer env >>= find k

get :: Key -> Env -> Maybe Value
get k env = find k env >>= Map.lookup k . inner

newEnv :: Env
newEnv = Env Map.empty Nothing

outerC :: Env -> Env
outerC env = Env Map.empty (Just env)

doBinds :: [Key] -> [Value] -> Env -> Env
doBinds (k:ks) (v:vs) env = doBinds ks vs (set k v env)
doBinds _ _ env = env
