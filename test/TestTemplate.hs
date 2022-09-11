module TestTemplate (tests) where

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Template tests"
    []