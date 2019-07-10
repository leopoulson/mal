import qualified System.Console.Haskeline as HL

import Types
import Reader
import Printer

type MalStr = String


malRead :: String -> MVal
malRead = readStr

malPrint :: MVal -> String
malPrint = prStr

malEval :: MVal -> MVal
malEval = id

rep :: MalStr -> String
rep = malPrint . malEval . malRead

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
