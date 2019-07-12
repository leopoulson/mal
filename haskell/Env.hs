module Env
  (Env, set, find, get, newEnv)
where


import Types

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type Key = String
type Value = MVal

data Env = Env {inner :: Map.Map Key Value, outer :: Maybe Env}
  deriving (Eq, Show)

set :: Env -> Key -> Value -> Env
set env k v = Env (Map.insert k v $ inner env) (outer env)

find :: Key -> Env -> Maybe Env
find k env = if Map.member k (inner env)
             then Just env
             else outer env >>= find k

get :: Key -> Env -> Value
get k env = fromMaybe (error $ "Key " ++ show k ++ " not in env.") $
            find k env >>= Map.lookup k . inner

newEnv :: Maybe Env -> Env
newEnv = Env Map.empty

-- m = newEnv Nothing
-- m' = set m "t" (MStr "t")
-- m'' = set m' "l" (MStr "l")
-- t = get "t" m''
-- t2 = get "l" m''
-- t3 = get "n" m''
