{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq.Parser 
    ( JqFilter(JqField, JqIndex, JqNil)
    , JqQuery(JqQueryObject, JqQueryArray, JqQueryFilter) 
    ) where

import Data.Text as T

data JqFilter
  = JqField Text JqFilter
  | JqIndex Int JqFilter
  | JqNil
  deriving (Show, Read, Eq)

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter s = undefined

data JqQuery
  = JqQueryObject [(Text, JqQuery)]
  | JqQueryArray [JqQuery]
  | JqQueryFilter JqFilter
  deriving (Show, Read, Eq)

