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

symbol :: Parser String
symbol = many1 $ oneOf "!#$%&|*+-/:<=>?@^_~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

str :: Parser String
str = many1 letter

readNum :: Parser MVal
readNum = MNum . read <$> many1 digit

readNegNum :: Parser MVal
readNegNum = f <$> char '-' <*> many1 digit
  where
    f sign number = MNum $ read $ sign : number

readSym :: Parser MVal
readSym = MSym <$> symbol

readString :: Parser MVal
readString = MStr <$> str

readNumber :: Parser MVal
readNumber = readNum
  <|> try readNegNum

readAtom :: Parser MVal
readAtom = readNumber
  <|> readSym
  -- <|> readString

readList :: Parser MVal
readList = MList <$> (char '(' *> whitespace *> readForm `sepEndBy` whitespace <* char ')') 
  
readForm :: Parser MVal
readForm = whitespace *> (readList
  <|> readAtom)

-- (1 2, 3,,,,),,
