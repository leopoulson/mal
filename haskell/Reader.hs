module Reader
  (readStr)
where

import Types
import Prelude hiding (readList)
  
import Text.ParserCombinators.Parsec (
    Parser, parse, char, digit, letter, try, chainl,
    (<|>), oneOf, noneOf, many, many1, skipMany, skipMany1, sepEndBy, sepEndBy1, string)

--Shorthand
--parse' p str = parse p "" str

readStr :: String -> MVal
readStr input = either (error . show) id $ parse readForm "" input

whitespace :: Parser ()
whitespace = skipMany $ oneOf  [' ', ',', '\n']

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

str :: Parser String
str = many1 letter

readNum :: Parser MVal
readNum = MNum . read <$> many1 digit

readSym :: Parser MVal
readSym = MSym <$> symbol

readString :: Parser MVal
readString = MStr <$> str

readAtom :: Parser MVal
readAtom = readNum
  <|> readSym
  <|> readString

readList :: Parser MVal
readList = MList <$> (char '(' *> whitespace *> readForm `sepEndBy` whitespace <* char ')') 
  
readForm :: Parser MVal
readForm = whitespace *> (readList
  <|> readAtom)

-- (1 2, 3,,,,),,
