module InterpreterSpec where
import SpecHelper

import Core (ScmValue(..), nullEnv)
import Interpreter (eval)

import Control.Monad.Except (runExceptT)

spec :: Spec
spec = do
  describe "eval" $ do
    context "No effect to environment" $ do
      it "String should get the same String" $ do
        env <- nullEnv
        r <- runExceptT $ eval env (String "a")
        case r of
          Right (String v) -> v `shouldBe` "a"
          _ -> undefined

main :: IO ()
main = hspec spec
