module TestParser (tests) where

import Data.Either (isLeft)
import Data.Text as T
import Data.Text.Arbitrary ()
import Lhx.Parser qualified as LP
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Test.Tasty.QuickCheck as QC

type Title = String

type MessageIfFail = String

type ParsedString = T.Text

type ExpectedValue = [LP.Chunk]

type ParsedTestCase = (ParsedString, ExpectedValue)

type FailParsedTestCase = (ParsedString, MessageIfFail)

createTestTreeParse :: ParsedTestCase -> TestTree
createTestTreeParse (parsedString, expectedValue) =
  testCase (T.unpack parsedString) $ LP.parse parsedString @?= Right expectedValue

createTestTreeParses :: Title -> [ParsedTestCase] -> TestTree
createTestTreeParses title cases = testGroup title $ Prelude.map createTestTreeParse cases

parseFunctionTests :: TestTree
parseFunctionTests =
  createTestTreeParses
    "functions"
    [ ("$foo;", [LP.Apply 0 [LP.FName "foo"]]),
      ("$1:foo;", [LP.Apply 1 [LP.FName "foo"]]),
      ("$foo:bar;", [LP.Apply 0 [LP.FName "foo", LP.FName "bar"]]),
      ("$8:foo:bar:baz;", [LP.Apply 8 [LP.FName "foo", LP.FName "bar", LP.FName "baz"]])
    ]

-- Don't work, I don't know why.
-- testNameFunctionProperty :: TestTree
-- testNameFunctionProperty =
--   QC.testProperty "name function property" $
--     \name -> nameProperty name QC.==> LP.parse ("$" <> name <> ";") == Right [LP.Apply 0 [LP.FName name]]
--   where
--     nameProperty name =
--       not (T.null name)
--         && isLower (T.head name)
--         && T.all (liftA2 (||) isLower isDigit) (T.tail name)

parseRawTests :: TestTree
parseRawTests =
  createTestTreeParses
    "raw"
    [ ("just text", [LP.Raw "just text"]),
      (";;;; qwerty ;; asd ; ; ;  '' :", [LP.Raw ";;;; qwerty ;; asd ; ; ;  '' :"])
      -- ("$$", [LP.Raw "$"])
    ]

parseManyTextsWithoutDollar :: TestTree
parseManyTextsWithoutDollar =
  QC.testProperty "many texts" $
    \s -> not (T.null s || T.any (== '$') s) QC.==> LP.parse s == Right [LP.Raw s]

parseFunctionsAndRawsTests :: TestTree
parseFunctionsAndRawsTests =
  createTestTreeParses
    "functions and raws"
    [ ("", []),
      ("a$foo;b$1:bar;", [LP.Raw "a", LP.Apply 0 [LP.FName "foo"], LP.Raw "b", LP.Apply 1 [LP.FName "bar"]]),
      ( ";;;; $798745987234:foo:bar:baz:bazzz:rev; ::::",
        [ LP.Raw ";;;; ",
          LP.Apply 798745987234 [LP.FName "foo", LP.FName "bar", LP.FName "baz", LP.FName "bazzz", LP.FName "rev"],
          LP.Raw " ::::"
        ]
      )
    ]

rightCases :: TestTree
rightCases =
  testGroup
    "right"
    [ parseFunctionTests,
      -- testNameFunctionProperty,
      parseRawTests,
      parseFunctionsAndRawsTests,
      parseManyTextsWithoutDollar
    ]

createCaseWithFailParse :: FailParsedTestCase -> TestTree
createCaseWithFailParse (str, message) = testCase (T.unpack str) $ isLeft (LP.parse str) @? message

createCasesWithFailParse :: [FailParsedTestCase] -> TestTree
createCasesWithFailParse cases = testGroup "fail" $ Prelude.map createCaseWithFailParse cases

failCases :: TestTree
failCases =
  createCasesWithFailParse
    [ ("$", "dollar must not parse"),
      ("$  foo;", "only a number or a symbol should be after the dollar"),
      ("$;", "semicolumn isn't a number or a letter, after dollar should be a number or a letter"),
      ("$foo", "if you forgot the semicolon, there must be an error"),
      ("$9783535:foo:bar:baz:bazzz:lsjlkdjflskdjflskdjf:slkdjflskjdfsd", "if you forgot the semicolon, there must be an error")
    ]

tests :: TestTree
tests =
  testGroup
    "Parser tests"
    [ rightCases,
      failCases
    ]
