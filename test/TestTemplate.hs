module TestTemplate (tests) where

import Data.Either (isLeft, isRight)
import Data.Either.Combinators (leftToMaybe, rightToMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (intercalate)
import Data.Text qualified as T
import Data.Text.Arbitrary ()
import Lhx (Separator (Separator))
import Lhx qualified as L
import Lhx.Parser qualified as LP
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?), (@?=))
import Test.Tasty.QuickCheck qualified as QS

makeTemplateTests :: TestTree
makeTemplateTests =
  testGroup
    "make template"
    [ testGroup
        "fail"
        [ testCase "parse errors remain" $
            isLeft (L.makeTemplate "$foo") @? "parsing error should remain"
        , testCase "unknown function" $
            isLeft (L.makeTemplate "$foo;") @? "function isn't in the list functions"
        , testCase "all unknown functions must be caught" $
            leftToMaybe (L.makeTemplate "$foo:bar:rev:baz:rev:bazzz:rev;")
              @?= Just
                [ L.Error "Unknown function: foo"
                , L.Error "Unknown function: bar"
                , L.Error "Unknown function: baz"
                , L.Error "Unknown function: bazzz"
                ]
        ]
    , testGroup
        "right"
        [ testCase "all good functions must be parsed" $
            isRight
              ( L.makeTemplate $
                  "$" <> intercalate ":" (map (LP.unFName . fst) L.functions) <> ";"
              )
              @? "don't apply function in the list functions"
        ]
    ]

makeInputTest :: TestTree
makeInputTest =
  testGroup
    "make input"
    [ testCase "empty" $ L.makeInput (L.Separator " ") "" @?= L.Input "" [""]
    , testCase "separator ','" $
        L.makeInput (L.Separator ",") "a, b, c" @?= L.Input "a, b, c" ["a", " b", " c"]
    ]

applyFunctionTest :: TestTree
applyFunctionTest =
  let reverseTemplate = L.makeTemplate "$rev;"
   in testGroup
        "Apply tests"
        [ QS.testProperty "reverse function" $
            \s -> fromMaybe False $
              rightToMaybe $ do
                template <- reverseTemplate
                text <- L.apply template (L.makeInput (Separator ",") s)
                pure $ text == T.reverse s
        ]

applyTemplateWithIndexes :: TestTree
applyTemplateWithIndexes =
  let makeAssert templateT inputString result =
        assertEqual (T.unpack templateT) (Right result) $ do
          template <- L.makeTemplate templateT
          L.apply template (L.makeInput (Separator ",") inputString)
   in testGroup
        "indexes"
        [ testCase "zero index is all input string" $
            makeAssert "$0:rev;" "abcd" "dcba"
        , testCase "swap 2 arguments" $
            makeAssert "$2,$1" "a,b" "b,a"
        , testCase "function with indexes" $
            makeAssert "$2:rev:rev;,$1:rev;" "abc,de" "de,cba"
        ]

tests :: TestTree
tests =
  testGroup
    "Template tests"
    [ makeTemplateTests
    , makeInputTest
    , applyFunctionTest
    , applyTemplateWithIndexes
    ]