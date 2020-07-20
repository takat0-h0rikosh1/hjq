{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq 
    ( parseJqFilter, parseJqQuery
    ) where

import Data.Text as T
import Data.Hjq.Parser
import Data.Attoparsec.Text
import Control.Applicative

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter s = showParseResult
 $ parse (jqFilterParser <* endOfInput) s `feed` ""

schar :: Char -> Parser Char
schar c = skipSpace *> char c <* skipSpace

jqFilterParser :: Parser JqFilter
jqFilterParser = schar '.' >> (jqFilter <|> jqIndex <|> pure JqNil)
  where
    jqFilter :: Parser JqFilter
    jqFilter
      = (schar '.' >> jqField) <|> jqIndex <|> pure JqNil

    jqField :: Parser JqFilter
    jqField = JqField <$> (word <* skipSpace) <*> jqFilter

    jqIndex :: Parser JqFilter
    jqIndex = JqIndex <$> (schar '[' *> decimal <* schar ']') <*> jqFilter

parseJqQuery :: Text -> Either Text JqQuery
parseJqQuery s = showParseResult $ parse (jqQueryParser <* endOfInput) s `feed` ""

jqQueryParser :: Parser JqQuery
jqQueryParser = queryArray <|> queryFilter <|> queryObject
  where
    queryArray :: Parser JqQuery
    queryArray = JqQueryArray <$> (schar '[' *> jqQueryParser `sepBy` (schar ',') <* schar ']')

    queryObject :: Parser JqQuery
    queryObject  = JqQueryObject <$> (schar '{' *> (qObj `sepBy` schar ',') <* schar '}')

    qObj :: Parser (Text, JqQuery)
    qObj = (,) <$> (schar '"' *> word <* schar '"') <*> (schar ':' *> jqQueryParser)

    queryFilter :: Parser JqQuery
    queryFilter = JqQueryFilter <$> jqFilterParser

showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack $ show r

word :: Parser Text
word = fmap pack $ many1 (letter <|> char '-' <|> char '_' <|> digit)
