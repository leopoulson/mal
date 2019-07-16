import qualified System.Console.Haskeline as HL

import Types
import Reader
import Printer
import Env

--import qualified Data.Map as Map

replEnv :: Env
replEnv = set "/" (MFun divd) .
          set "*" (MFun mul) .
          set "-" (MFun sub) .
          set "+" (MFun add) $ newEnv

collapseList :: ([MVal] -> MVal) -> [MVal] -> MVal
collapseList _ [MNum n] = MNum n
collapseList f (x : y : rest) = collapseList f (f [x, y] : rest)
collapseList _ _ = error "Incorrect types for collapse list"

applyList :: Env -> MVal -> MVal
applyList _ (MList (MFun op : rest)) = collapseList op rest
-- applyList _ (MList (MSym "def!" : k : v : rest)) = 
applyList _ mv = mv

eval :: Env -> MVal -> MVal
eval env (MList ms) = applyList env $ evalAST env (MList ms)
eval env mv = evalAST env mv

evalAST :: Env -> MVal -> MVal
evalAST env (MList ms) = MList $ map (eval env) ms
evalAST env (MSym op) = get op env
evalAST _ mv = mv

add, sub, divd, mul :: [MVal] -> MVal
add [MNum x, MNum y] = MNum $ x + y
add _ = error "Incorrect arguments to add."
sub [MNum x, MNum y] = MNum $ x - y
sub _ = error "Incorrect arguments to sub."
mul [MNum x, MNum y] = MNum $ x * y
mul _ = error "Incorrect arguments to mul."
divd [MNum x, MNum y] = MNum $ x `Prelude.div` y
divd _ = error "Incorrect arguments to div."

-- -------

malRead :: (String, Env) -> (MVal, Env)
malRead (st, env) = (readStr st, env) 

malPrint :: (MVal, Env) -> (String, Env)
malPrint (val, env) = (prStr val, env) 

malEval :: (MVal, Env) -> (MVal, Env)
malEval (val, env) = (eval env val, env)

rep :: (String, Env) -> (String, Env)
rep = malPrint . malEval . malRead

repl :: HL.InputT IO ()
repl = do
  line <- HL.getInputLine "user> "
  case line of
    Nothing -> return ()
    Just "" -> repl
    Just ":q" -> return ()
    Just input -> do
      HL.outputStrLn . fst $ rep (input, replEnv)
      repl

main :: IO ()
main = HL.runInputT HL.defaultSettings repl
