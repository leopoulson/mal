import qualified System.Console.Haskeline as HL

type MalStr = String


malRead :: MalStr -> String
malRead str = str


malPrint :: MalStr -> String
malPrint str = str


malEval :: MalStr -> String
malEval str = str


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
