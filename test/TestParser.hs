module TestParser (tests) where

import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))

import Lhx.Parser qualified as LP

(?=>) :: Text -> [LP.Chunk] -> TestTree
input ?=> chunks =
  testCase (T.unpack input) $ LP.parse input @?= Right chunks

functionCallParsingTests :: TestTree
functionCallParsingTests =
  testGroup
    "Functions"
    [ "$foo;" ?=> [LP.Apply 0 [LP.FName "foo"]]
    , "$1:foo;" ?=> [LP.Apply 1 [LP.FName "foo"]]
    , "$foo:bar;" ?=> [LP.Apply 0 [LP.FName "foo", LP.FName "bar"]]
    , "$8:foo1:bar2:baz3;" ?=>
        [LP.Apply 8 [LP.FName "foo1", LP.FName "bar2", LP.FName "baz3"]]
    , "$a1s2d3;" ?=> [LP.Apply 0 [LP.FName "a1s2d3"]]
    , "$4abc" ?=> [LP.Apply 4 [], LP.Raw "abc"]
    , "$+7" ?=> [LP.Apply 7 []]
    , "$-7" ?=> [LP.Apply (-7) []]
    , "$-0" ?=> [LP.Apply 0 []]
    , "$-12:foo:bar:baz;" ?=> [LP.Apply (-12) [LP.FName "foo", LP.FName "bar", LP.FName "baz"]]
    ]

rawTextParsingTests :: TestTree
rawTextParsingTests =
  testGroup
    "Raw text"
    [ "just text" ?=> [LP.Raw "just text"]
    , ";;;; qwerty ;; asd ; ; ;  '' :" ?=>
        [LP.Raw ";;;; qwerty ;; asd ; ; ;  '' :"]
    , "$$" ?=> [LP.Raw "$"]
    ]

complexTemplateParsingTests :: TestTree
complexTemplateParsingTests =
  testGroup
    "Complex templates"
    [ "a$foo;b$1:bar;" ?=>
        [ LP.Raw "a", LP.Apply 0 [LP.FName "foo"]
        , LP.Raw "b", LP.Apply 1 [LP.FName "bar"]
        ]
    , "; $798745987234:foo:bar:baz:bazzz:rev;; :  " ?=>
      [ LP.Raw "; "
      , LP.Apply 798745987234
          [ LP.FName "foo", LP.FName "bar", LP.FName "baz"
          , LP.FName "bazzz", LP.FName "rev"
          ]
      , LP.Raw "; :  "
      ]
    ]

correctTemplatesTests :: TestTree
correctTemplatesTests =
  testGroup
    "right"
    [ functionCallParsingTests
    , rawTextParsingTests
    , complexTemplateParsingTests
    ]

(@!!) :: Text -> String -> TestTree
input @!! message =
  testCase (T.unpack input) $ isLeft (LP.parse input) @? message

brokenTemplateTests :: TestTree
brokenTemplateTests =
  testGroup
    "Broken templates"
    [ "$" @!! "Sole $ shouldn't be parsed"
    , "$  foo;" @!! "Function name cannot contain non alphanumeric chars"
    , "$;" @!! "Function name cannot be empty"
    , "$12:4abc;" @!! "Function name cannot start with a digit"
    , "$foo" @!! "Even a sole function call should be terminated with ;"
    , "$9783535:foo:bar:baz:bazzz:a:b:c:d:e"
        @!! "Any complex call should be terminated with ;"
    , "$- 12" @!! "There must not be a space after the sign"
    ]

tests :: TestTree
tests =
  testGroup
    "Parser tests"
    [ correctTemplatesTests
    , brokenTemplateTests
    ]
