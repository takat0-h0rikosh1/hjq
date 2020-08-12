{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq 
    ( parseJqFilter, parseJqQuery, unsafeParseFilter, applyFilter
    ) where

import Data.Text as T
import Data.Hjq.Parser
import Data.Hjq.Query
import Data.Attoparsec.Text
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Data.Aeson
import Data.Aeson.Lens
import Control.Monad.Either

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter s = showParseResult
 $ parse (jqFilterParser <* endOfInput) s `feed` ""

schar :: Char -> Parser Char
schar c = skipSpace *> char c <* skipSpace

jqFilterParser :: Parser JqFilter
jqFilterParser = schar '.' >> (jqField <|> jqIndex <|> pure JqNil)
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

showParseResult :: Show a => Data.Attoparsec.Text.Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack $ show r

word :: Parser Text
word = fmap pack $ many1 (letter <|> char '-' <|> char '_' <|> digit)

unsafeParseFilter :: Text -> JqFilter
unsafeParseFilter t = case parseJqFilter t of
  Right f -> f
  Left s -> error $ "PARSE FAILURE IN A TEST:" ++ unpack s

applyFilter :: JqFilter -> Value -> Either T.Text Value
applyFilter (JqField fieldName n) obj@(Object _)
  = join $ noteNotFoundError fieldName (fmap (applyFilter n) (obj V.!? key fieldName))
applyFilter (JqIndex index n) array@(Array _)
  = join $ noteOutOfRangeError index (fmap (applyFilter n) (array V.!? nth index))
applyFilter JqNil v = Right v
applyFilter f o = Left $ "unexpected pattern:" <> tshow f <> ":" <> tshow o

noteNotFoundError :: T.Text -> Maybe a -> Either T.Text a
noteNotFoundError _ (Just x) = Right x
noteNotFoundError s Nothing = Left $ "out of range:" <> tshow s

noteOutOfRangeError :: Int -> Maybe a -> Either T.Text a
noteOutOfRangeError _ (Just x) = Right x
noteOutOfRangeError s Nothing = Left $ "out of range:" <> tshow s

tshow :: Show a => a -> T.Text
tshow = T.pack . show
