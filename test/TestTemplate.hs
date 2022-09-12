module TestTemplate (tests) where

import Data.Either (isLeft)
import Data.Either.Combinators (isRight, leftToMaybe, rightToMaybe)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Arbitrary ()
import Lhx (Separator (Separator))
import Lhx qualified as L
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Test.Tasty.QuickCheck qualified as QS

type FunctionName = T.Text

makeTemplateTests :: TestTree
makeTemplateTests =
  testGroup
    "make template"
    [ testGroup
        "fail"
        [ testCase "parse errors remain" $ isLeft (L.makeTemplate "$foo") @? "parsing error should remain"
        , testCase "unknown function" $ isLeft (L.makeTemplate "$foo;") @? "function isn't in the list functions"
        , testCase "all unknown functions must be caught" $
            leftToMaybe (L.makeTemplate "$foo:bar:rev:baz:strip:lstrip:bazzz;")
              @?= Just
                [ L.Error "Unknown function: foo"
                , L.Error "Unknown function: bar"
                , L.Error "Unknown function: baz"
                , L.Error "Unknown function: bazzz"
                ]
        ]
    , testGroup
        "right"
        [ testCase "all good functions" $
            isRight (L.makeTemplate "$rev:strip:lstrip:rstrip;")
              @? "don't apply function in the list functions"
        ]
    ]

createTestPropertyFunction :: (FunctionName, Char, T.Text -> T.Text) -> TestTree
createTestPropertyFunction (name, sep, f) = QS.testProperty (T.unpack name) $
  \s -> not (T.any (== sep) s)
    QS.==> fromMaybe False
    $ rightToMaybe $ do
      template <- L.makeTemplate ("$" <> name <> ";")
      text <- L.apply template (L.makeInput (Separator $ T.singleton sep) s)
      pure $ text == f s

createTestPropertiesFunction :: [(FunctionName, Char, T.Text -> T.Text)] -> TestTree
createTestPropertiesFunction functions =
  testGroup "apply template" $
    Prelude.map createTestPropertyFunction functions

applyTemplateTests :: TestTree
applyTemplateTests =
  createTestPropertiesFunction
    [ ("rev", ' ', T.reverse)
    , ("strip", ',', T.strip)
    , ("lstrip", ',', T.stripStart)
    , ("rstrip", ',', T.stripEnd)
    , ("rev:rev", ' ', id)
    , ("lstrip:rev", ',', T.reverse . T.stripStart)
    ]

applyTemplateWithIndexes :: TestTree
applyTemplateWithIndexes =
  testGroup
    "indexes"
    []

-- testCase "" $ assertEqual "$1 $0" (Right "b a") $ do
--   template <- L.makeTemplate "$1 $0"
--   L.apply template (L.makeInput (Separator ",") "a,b")

tests :: TestTree
tests =
  testGroup
    "Template tests"
    [ makeTemplateTests
    , applyTemplateTests
    , applyTemplateWithIndexes
    ]