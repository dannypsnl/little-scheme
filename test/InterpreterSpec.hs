module InterpreterSpec where
import SpecHelper

import Core (ScmValue(..), nullEnv)
import Interpreter (eval)

import Control.Monad.Except (runExceptT)
import Data.Either (fromRight)

spec :: Spec
spec = do
  describe "eval" $ do
    context "No effect to environment" $ do
      it "String should get the same String" $ do
        env <- nullEnv
        r <- runExceptT $ eval env (String "a")
        r `shouldBe` Right (String "a")
    context "Effect environment" $ do
      it "let* allow second binding is done in an env in which first binding is visible" $ do
        env <- nullEnv
        r <- runExceptT $ eval env (List (Atom "let*" : List [ List [ Atom "x", Number 1 ], List [ Atom "y", Atom "x" ] ] : [Atom "y"]))
        r `shouldBe` Right (Number 1)

main :: IO ()
main = hspec spec
