module Reader where

import Types
import Text.ParserCombinators.Parsec (
    Parser, parse, char, digit, letter, try,
    (<|>), oneOf, noneOf, many, many1, skipMany, skipMany1, sepEndBy, string)
 
--Shorthand 
--parse' p str = parse p "" str

readStr :: String -> Maybe MVal
readStr str = undefined

whitespace :: Parser ()
whitespace = skipMany1 $ oneOf " \n"

readNum :: Parser MVal
readNum = MNum . read <$> many1 digit
