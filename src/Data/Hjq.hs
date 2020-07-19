{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq 
    ( parseJqFilter
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

showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack $ show r

word :: Parser Text
word = fmap pack $ many1 (letter <|> char '-' <|> char '_' <|> digit)
