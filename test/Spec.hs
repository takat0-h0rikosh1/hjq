import Test.HUnit
import Data.Hjq
import Data.Hjq.Parser
import Data.Text as T

main :: IO ()
main = do
  _ <- runTestTT $ TestList
    [TestLabel "Test jqFilterParse" jqFilterParseTest]
  return()

jqFilterParseTest :: Test
jqFilterParseTest = TestList
  ["jqFilterParser test 1" ~: parseJqFilter (pack ".") ~?= Right JqNil]

