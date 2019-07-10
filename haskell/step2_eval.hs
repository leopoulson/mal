import qualified System.Console.Haskeline as HL

import Types
import Reader
import Printer

import qualified Data.Map as Map

type EnvMap = Map.Map Char (Int -> Int -> Int)

replEnv :: EnvMap
replEnv = Map.fromList [('+', (+)), ('-', (-)), ('/', div), ('*', (*))]

envLookup :: EnvMap -> Char -> (Int -> Int -> Int)
envLookup m k = Map.findWithDefault (error ("Unsupported symbol" ++ [k])) k m

collapseList :: (Int -> Int -> Int) -> [MVal] -> MVal
collapseList _ [MNum n] = MNum n
collapseList f (MNum x : MNum y : rest) = collapseList f (MNum (f x y) : rest)
collapseList _ _ = error "Incorrect types for collapse list"

applyList :: EnvMap -> MVal -> MVal
applyList env (MList (MSym op : rest)) = collapseList symbol rest
  where
    symbol = envLookup env op
applyList _ mv = mv

eval :: EnvMap -> MVal -> MVal
eval env (MList ms) = applyList env $ evalAST env (MList ms)
eval _ mv = mv

evalAST :: EnvMap -> MVal -> MVal
evalAST env (MList ms) = MList $ map (eval env) ms
evalAST _ mv = mv

-- -------

malRead :: String -> MVal
malRead = readStr

malPrint :: MVal -> String
malPrint = prStr

malEval :: EnvMap -> MVal -> MVal
malEval map = eval map 

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
