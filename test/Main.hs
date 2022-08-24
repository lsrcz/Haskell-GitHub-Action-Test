module Main where

import Runner
import qualified Spec

main :: IO ()
main = runTests "tests" Spec.spec
