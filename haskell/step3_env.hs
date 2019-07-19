import qualified System.Console.Haskeline as HL

import Types
import Reader
import Printer
import Env

import Debug.Trace as D

replEnv :: Env
replEnv = set (MSym "def!") (MSym "def!") . 
          set (MSym "/") (MFun divd) .
          set (MSym "*") (MFun mul) .
          set (MSym "-") (MFun sub) .
          set (MSym "+") (MFun add) $ newEnv

add, sub, divd, mul :: [MVal] -> MVal
add [MNum x, MNum y] = MNum $ x + y
add _ = error "Incorrect arguments to add."
sub [MNum x, MNum y] = MNum $ x - y
sub _ = error "Incorrect arguments to sub."
mul [MNum x, MNum y] = MNum $ x * y
mul _ = error "Incorrect arguments to mul."
divd [MNum x, MNum y] = MNum $ x `Prelude.div` y
divd _ = error "Incorrect arguments to div."

--

collapseList :: ([MVal] -> MVal) -> [MVal] -> MVal
collapseList _ [MNum n] = MNum n
collapseList f (x : y : rest) = collapseList f (f [x, y] : rest)
collapseList _ _ = error "Incorrect types for collapse list"

applyList :: Env -> MVal -> (MVal, Env)
applyList env (MList (MFun op : rest)) = (collapseList op rest, env)
applyList env mv = (mv, env)

eval :: Env -> MVal -> (MVal, Env)
eval env (MList (MSym "def!" : key : val : rest)) =
  case val' of
    MList (MErr err : _) -> trace "error" (MErr err, env)
    _                    -> (MList (val' : rest), set key val' env)
  where val' = fst $ eval env val
eval env (MList ms) = applyList env $ evalAST env (MList ms)
eval env mv = (evalAST env mv, env)

evalAST :: Env -> MVal -> MVal
evalAST env (MList ms) = MList $ map (fst . eval env) ms
evalAST env op@(MSym str) = case get op env of
                              Nothing -> MErr $ "'" ++ str ++ "' not found."
                              Just val -> val
evalAST _ mv = mv

----------------

malRead :: (String, Env) -> (MVal, Env)
malRead (st, env) = (readStr st, env) 

malPrint :: (MVal, Env) -> (String, Env)
malPrint (val, env) = (prStr val, env) 

malEval :: (MVal, Env) -> (MVal, Env)
malEval (val, env) = eval env val

rep :: (String, Env) -> (String, Env)
rep = malPrint . malEval . malRead

repl :: Env -> HL.InputT IO ()
repl env = do
  line <- HL.getInputLine "user> "
  case line of
    Nothing -> return ()
    Just "" -> repl env
    Just ":q" -> return ()
    Just input -> do
      let (mval, env') = rep (input, env)
      HL.outputStrLn mval
      repl env'

main :: IO ()
main = HL.runInputT HL.defaultSettings (repl replEnv)
