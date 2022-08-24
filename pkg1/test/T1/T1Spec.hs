module T1.T1Spec where

import T1.T1
import Test.Hspec

spec :: Spec
spec = do
  describe "T1" $ do
    it "T1" $ do
      f1 2 2 `shouldBe` 4
