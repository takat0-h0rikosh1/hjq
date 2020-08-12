{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Data.Hjq
import Data.Hjq.Parser
import Data.Text()
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Data.Aeson
import Data.Aeson.Lens

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
    parseJqQuery "{\"hoge\":[],\"piyo\":[]}" ~?=
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
    parseJqQuery "{ \"hoge\" : [] , \"piyo\" : [] }" ~?=
      Right (JqQueryObject [("hoge", JqQueryArray []), ("piyo", JqQueryArray[])])
  ]

testData :: Value
testData = Object $ H.fromList
  [ ("string-field", String "string value")
  , ("nested-field", Object $ H.fromList
      [ ("inner-string", String "inner value")
      , ("inner-number", Number 100)
      ]
    )
  , ("array-field", Array $ V.fromList
      [ String "first field"
      , String "next field"
      , Object (H.fromList
        [ ("object-in-array", String "string value in object-in-array") ] )
      ]
    )
  ]

applyFilterTest :: Test
applyFilterTest = TestList
  ["applyFilter test 1" ~: applyFilter (unsafeParseFilter ".") testData ~?= Right testData
  , "applyFilter test 2" ~:
    (Just $ applyFilter (unsafeParserFilter ".string-field") testData)
    ~?= fmap Right (testData^?key "string-field")
  , "applyFilter test 3" ~:
    (Just $ applyFilter (unsafeParseFilter ".nested-field.inner-string") testData)
    ~?= fmap Right (testData ^? key "nested-field" . key "inner-string")
  , "applyFilter test 4" ~:
    (Just $ applyFilter (unsafeParseFilter ".nested-field.inner-number") testData)
    ~?= fmap Right (testData ^? key "nested-field" . key "inner-number")
  , "applyFilter test 5" ~:
    (Just $ applyFilter (unsafeParserFilter ".array-field[2].object-in-array") testData)
    ~?= fmap Right (testData ^? key "array-field" . nth 2 . key "object-in-array")
  ]
