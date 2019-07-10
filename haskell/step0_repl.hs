import qualified System.Console.Haskeline as HL

type MalStr = String


mal_read :: MalStr -> String
mal_read str = str


mal_print :: MalStr -> String
mal_print str = str


mal_eval :: MalStr -> String
mal_eval str = str


rep :: MalStr -> String
rep = mal_print . mal_eval . mal_read

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
