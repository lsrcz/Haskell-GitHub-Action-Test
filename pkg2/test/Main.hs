module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.Runners.Reporter as Reporter

tests =
  testGroup
    "Unit tests"
    [ testCase "List comparison (different length)" $
        [1, 2, 3] `compare` [1, 2] @?= GT,
      -- the following test does not hold
      testCase "List comparison (same length)" $
        [1, 2, 3] `compare` [1, 2, 2] @?= LT
    ]

main :: IO ()
main = defaultMainWithIngredients [Reporter.ingredient] tests
