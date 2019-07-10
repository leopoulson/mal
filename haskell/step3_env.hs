import qualified System.Console.Haskeline as HL

import Types
import Reader
import Printer

import qualified Data.Map as Map

type EnvMap = Map.Map Char MVal

replEnv :: EnvMap
replEnv = Map.fromList [('+', MFun add), ('-', MFun sub), ('/', MFun Main.div), ('*', MFun mul)]

envLookup :: EnvMap -> Char -> MVal
envLookup m k = Map.findWithDefault (error ("Unsupported symbol" ++ [k])) k m

collapseList :: ([MVal] -> MVal) -> [MVal] -> MVal
collapseList _ [MNum n] = MNum n
collapseList f (x : y : rest) = collapseList f (f [x, y] : rest)
collapseList _ _ = error "Incorrect types for collapse list"

applyList :: EnvMap -> MVal -> MVal
applyList _ (MList (MFun op : rest)) = collapseList op rest
applyList _ mv = mv

eval :: EnvMap -> MVal -> MVal
eval env (MList ms) = applyList env $ evalAST env (MList ms)
eval _ mv = mv

evalAST :: EnvMap -> MVal -> MVal
evalAST env (MList ms) = MList $ map (eval env) ms
evalAST env (MSym op) = envLookup env op
evalAST _ mv = mv

add, sub, div, mul :: [MVal] -> MVal
add [MNum x, MNum y] = MNum $ x + y
add _ = error "Incorrect arguments to add."
sub [MNum x, MNum y] = MNum $ x - y
sub _ = error "Incorrect arguments to sub."
mul [MNum x, MNum y] = MNum $ x * y
mul _ = error "Incorrect arguments to mul."
div [MNum x, MNum y] = MNum $ x `Prelude.div` y
div _ = error "Incorrect arguments to div."
-- -------


malRead :: String -> MVal
malRead = readStr

malPrint :: MVal -> String
malPrint = prStr

malEval :: EnvMap -> MVal -> MVal
malEval = eval 

rep :: String -> String
rep = malPrint . malEval replEnv . malRead

repl :: HL.InputT IO ()
repl = do
  line <- HL.getInputLine "user> "
  case line of
    Nothing -> return ()
    Just "" -> repl
    Just ":q" -> return ()
    Just input -> do
      HL.outputStrLn $ rep input
      repl

main :: IO ()
main = HL.runInputT HL.defaultSettings repl
