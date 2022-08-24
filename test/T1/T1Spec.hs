module T1.T1Spec where

import Test.Hspec
import T1.T1

spec :: Spec
spec = do
  describe "T1" $ do
    it "T1" $ do
      f1 2 2 `shouldBe` 4
