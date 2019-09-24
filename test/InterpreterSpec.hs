module InterpreterSpec where

import SpecHelper

spec :: Spec
spec = do
  describe "test spec" $ do
    context "add 1 2 should be 3" $ do
      it "1 + 2 = 3" $ do
        1 + 2 `shouldBe` 3

main :: IO ()
main = hspec spec
