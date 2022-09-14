module Main where

import Test.Tasty (defaultMain, testGroup)
import TestParser qualified
import TestTemplate qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "Lhx tests"
      [ TestParser.tests
      , TestTemplate.tests
      ]
