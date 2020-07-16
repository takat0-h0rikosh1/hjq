module Data.Hjq 
    ( parseJqFilter
    ) where

import Data.Text as T
import Data.Hjq.Parser

parseJqFilter :: Text -> Either String JqFilter
parseJqFilter _ = Right JqNil

