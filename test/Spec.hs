{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Data.Hjq
import Data.Hjq.Parser
import Data.Text()

main :: IO ()
main = do
  _ <- runTestTT $ TestList
    [TestLabel "Test jqFilterParse" jqFilterParseTest
    , TestLabel "Test jqFilterSpaces" jqFilterParserSpacesTest
    , TestLabel "Test jqQueryParserTest" jqQueryParserTest
    , TestLabel "Test jqQueryParserSpacesTest" jqQueryParserSpacesTest
    ]
  return()

jqFilterParseTest :: Test
jqFilterParseTest = TestList
  ["jqFilterParser test 1" ~: 
    parseJqFilter "." ~?= Right JqNil
  , "jqFilterParser test 2" ~:
      parseJqFilter ".[0]" ~?= Right (JqIndex 0 JqNil)
  , "jqFilterParser test 3" ~:
      parseJqFilter ".fieldName" ~?= Right (JqField "fieldName" JqNil)
  , "jqFilterParser test 4" ~:
      parseJqFilter ".[0].fieldName" ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
  , "JqFilterParser test 5" ~:
      parseJqFilter ".fieldName[0]" ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
  ]

jqFilterParserSpacesTest :: Test
jqFilterParserSpacesTest = TestList
  ["jqFilterParser spaces test 1" ~:
    parseJqFilter " . " ~?= Right JqNil
  , "jqFilterParser spaces test 2" ~:
    parseJqFilter " . [ 0 ] " ~?= Right (JqIndex 0 JqNil)
  , "jqFilterParser spaces test 3" ~:
    parseJqFilter " . fieldName " ~?= Right (JqField "fieldName" JqNil) 
  , "jqFilterParser test 4" ~:
      parseJqFilter " . [ 0 ] . fieldName " ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
  , "JqFilterParser test 5" ~:
      parseJqFilter " . fieldName [ 0 ] " ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
  ]

jqQueryParserTest :: Test
jqQueryParserTest = TestList
  ["jqQueryParser test 1" ~:
    parseJqQuery "[]" ~?= Right (JqQueryArray [])
  , "jqQueryParser test2" ~:
    parseJqQuery " [.hoge,.piyo]" ~?=
      Right(JqQueryArray [JqQueryFilter (JqField "hoge" JqNil), JqQueryFilter (JqField "piyo" JqNil)])
  ,"jqQueryParser test 3" ~:
    parseJqQuery "{\"hige\":[],\"piyo\":[]}" ~?=
      Right (JqQueryObject [("hoge", JqQueryArray []), ("piyo", JqQueryArray[])])
  ]

jqQueryParserSpacesTest :: Test
jqQueryParserSpacesTest = TestList
  ["jqQueryParser spaces test 1" ~:
    parseJqQuery " [] " ~?= Right (JqQueryArray [])
  , "jqQueryParser spaces test2" ~:
    parseJqQuery " [ . hoge , . piyo ] " ~?=
      Right(JqQueryArray [JqQueryFilter (JqField "hoge" JqNil), JqQueryFilter (JqField "piyo" JqNil)])
  ,"jqQueryParser spaces test 3" ~:
    parseJqQuery "{ \"hige\" : [] , \"piyo\" : [] }" ~?=
      Right (JqQueryObject [("hoge", JqQueryArray []), ("piyo", JqQueryArray[])])
  ]


