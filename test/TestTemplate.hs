module TestTemplate (tests) where

import Data.Bifunctor (first)
import Data.Either (isLeft, isRight, fromRight)
import Data.Text qualified as T
import Data.Text.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Test.Tasty.QuickCheck qualified as QC

import Lhx (Separator(..))
import Lhx qualified
import Lhx.Parser qualified as LP

templateMakingTests :: TestTree
templateMakingTests =
  testGroup
    "Making of templates"
    [ testGroup
        "Broken templates"
        [ testCase "Unclosed function call" $
            isLeft (Lhx.makeTemplate "$foo")
            @? "Shouldn't accept non-closed function call"
        , testCase "Unknown function" $
            isLeft (Lhx.makeTemplate "$foo;")
            @? "Shouldn't accept unknown function"
        , testCase "Several unknown functions" $
            case Lhx.makeTemplate "$foo:bar:rev:baz:rev:bazzz:rev;" of
              Right _ -> error "impossible"
              Left e  -> Lhx.errorText e @?= T.unlines
                [ "Unknown function: foo"
                , "Unknown function: bar"
                , "Unknown function: baz"
                , "Unknown function: bazzz"
                ]
        ]
    , testGroup
        "Correct templates"
        [ testCase "All known functions" $
            isRight (Lhx.makeTemplate $ mconcat
              [ "$"
              , T.intercalate ":" $ map (LP.unFName . fst) Lhx.functions
              , ";"
              ]) @? "Should accept any registered function"
        ]
    ]

inputMakingTests :: TestTree
inputMakingTests =
  testGroup
    "Make input"
    [ testCase "Separating by ','" $
        Lhx.iFields (Lhx.makeInput (Lhx.Separator ",") "a, b, c")
        @?= ["a", " b", " c"]
    ]

functionPropertyTests :: TestTree
functionPropertyTests =
  testGroup
    "Function properties"
    [ QC.testProperty "'rev' function" \s ->
        fromRight False do
          template <- Lhx.makeTemplate "$rev;"
          result <- Lhx.apply template (Lhx.makeInput (Separator ",") s)
          pure $ result == T.reverse s
    ]

indexingTests :: TestTree
indexingTests =
  testGroup
    "Indexing"
    [ testCase "Zero index should capture the whole input" $
        "$0" `appliedTo` "abcd" @?= Right "abcd"
    , testCase "Two fields should be swaped" $
        "$2,$1" `appliedTo` "a,b" @?= Right "b,a"
    , testCase "Functions should work well with indices" $
        "$2:rev:rev;,$1:rev;" `appliedTo` "abc,de" @?= Right "de,cba"
    , testCase "Index out of range" $
        "$20" `appliedTo` "a,b,c" @?= Left "Index is out of range: 20\n"
    , testCase "Negate index -1" $
        "$-1" `appliedTo` "a,b,c,d,e,f" @?= Right "f"
    , testCase "Negate index -3" $
        "$-3" `appliedTo` "a,b,c,d,e,f" @?= Right "d"
    , testCase "Negate index out of range" $
        "$-3" `appliedTo` "a,b" @?= Left "Index is out of range: -3\n"
    , testCase "To apply the function on the negate index" $
        "$-3:rev;" `appliedTo` "a,b,c,abcd,e,f" @?= Right "dcba"
    ]
 where
  appliedTo templateT inputString = first Lhx.errorText do
    template <- Lhx.makeTemplate templateT
    Lhx.apply template (Lhx.makeInput (Separator ",") inputString)

tests :: TestTree
tests =
  testGroup
    "Templating tests"
    [ templateMakingTests
    , inputMakingTests
    , functionPropertyTests
    , indexingTests
    ]
