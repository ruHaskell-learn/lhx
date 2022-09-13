module TestParser (tests) where

import Data.Either (isLeft)
import Data.Text as T
import Data.Text.Arbitrary ()
import Lhx.Parser qualified as LP
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))

type Title = String

type MessageIfFail = String

type ParsedString = T.Text

type ParsedTestCase = (ParsedString, [LP.Chunk])

type FailParsedTestCase = (ParsedString, MessageIfFail)

makeParsedTestCaseTestTrees :: Title -> [ParsedTestCase] -> TestTree
makeParsedTestCaseTestTrees title cases =
  testGroup title $
    Prelude.map
      ( \(parsedString, expectedValue) ->
          testCase (T.unpack parsedString) $
            LP.parse parsedString @?= Right expectedValue
      )
      cases

parseFunctionTests :: TestTree
parseFunctionTests =
  makeParsedTestCaseTestTrees
    "functions"
    [ ("$foo;", [LP.Apply 0 [LP.FName "foo"]])
    , ("$1:foo;", [LP.Apply 1 [LP.FName "foo"]])
    , ("$foo:bar;", [LP.Apply 0 [LP.FName "foo", LP.FName "bar"]])
    , ("$8:foo1:bar2:baz3;", [LP.Apply 8 [LP.FName "foo1", LP.FName "bar2", LP.FName "baz3"]])
    , ("$a1s2d3;", [LP.Apply 0 [LP.FName "a1s2d3"]])
    , ("$4abc", [LP.Apply 4 [], LP.Raw "abc"])
    ]

parseRawTests :: TestTree
parseRawTests =
  makeParsedTestCaseTestTrees
    "raw"
    [ ("just text", [LP.Raw "just text"])
    , (";;;; qwerty ;; asd ; ; ;  '' :", [LP.Raw ";;;; qwerty ;; asd ; ; ;  '' :"])
    , ("$$", [LP.Raw "$"])
    ]

parseFunctionsAndRawsTests :: TestTree
parseFunctionsAndRawsTests =
  makeParsedTestCaseTestTrees
    "functions and raws"
    [ ("", [])
    , ("a$foo;b$1:bar;", [LP.Raw "a", LP.Apply 0 [LP.FName "foo"], LP.Raw "b", LP.Apply 1 [LP.FName "bar"]])
    ,
      ( "; $798745987234:foo:bar:baz:bazzz:rev;; :  "
      ,
        [ LP.Raw "; "
        , LP.Apply 798745987234 [LP.FName "foo", LP.FName "bar", LP.FName "baz", LP.FName "bazzz", LP.FName "rev"]
        , LP.Raw "; :  "
        ]
      )
    ]

rightCases :: TestTree
rightCases =
  testGroup
    "right"
    [ parseFunctionTests
    , parseRawTests
    , parseFunctionsAndRawsTests
    ]

makeCasesWithFailParseTest :: [FailParsedTestCase] -> TestTree
makeCasesWithFailParseTest cases =
  testGroup "fail" $
    Prelude.map
      ( \(str, message) ->
          testCase (T.unpack str) $ isLeft (LP.parse str) @? message
      )
      cases

failCases :: TestTree
failCases =
  makeCasesWithFailParseTest
    [ ("$", "dollar must not parse")
    , ("$  foo;", "only a number or a symbol should be after the dollar")
    , ("$;", "semicolumn isn't a number or a letter, after dollar should be a number or a letter")
    , ("$foo", "if you forgot the semicolon, there must be an error")
    , ("$9783535:foo:bar:baz:bazzz:a:b:c:d:e", "if you forgot the semicolon, there must be an error")
    , ("$12:4abc;", "function name doesn't start with digit")
    ]

tests :: TestTree
tests =
  testGroup
    "Parser tests"
    [ rightCases
    , failCases
    ]
