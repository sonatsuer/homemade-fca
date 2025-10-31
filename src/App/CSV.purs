module App.CSV (parseCSV, CSV) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.List.Types (NonEmptyList, List)
import Data.String.CodeUnits (fromCharArray)
import StringParser (Parser, char, eof, noneOf, runParser, string)
import StringParser.Combinators (between, many, sepBy1, sepEndBy1)

type CSV = NonEmptyList (NonEmptyList String)

parseCSV :: String -> Either String CSV
parseCSV raw = lmap errorToString (runParser csvParser raw)
  where
  errorToString err = err.error <> " (Position: " <> show err.pos <> ")"

csvParser :: Parser CSV
csvParser = fileP <* eof
  where
  fileP = rowP `sepEndBy1` char '\n'
  rowP = fieldP `sepBy1` char ','
  fieldP = quotedParser <|> unquotedFieldParser <|> string ""

quotedParser :: Parser String
quotedParser = between (char '"') (char '"') $
  listToString <$> many quotedCharParser

quotedCharParser :: Parser Char
quotedCharParser = doubleQuoteParser <|> noQuoteParser
  where
  doubleQuoteParser = string "\"\"" $> '"'
  noQuoteParser = noneOf [ '"' ]

unquotedFieldParser :: Parser String
unquotedFieldParser = listToString <$>
  many (noneOf [ '\n', '"', ',' ])

listToString :: List Char -> String
listToString = fromFoldable >>> fromCharArray
