module InterpreterSpec where
import SpecHelper

import Core (ScmError, ScmValue(..), liftThrows, nullEnv)
import Interpreter (eval, primitiveBindings)
import Parser (readExpr)

import Control.Monad.Except (runExceptT)
import Data.Either (isRight)

spec :: Spec
spec = describe "eval" $ do
  context "No effect to environment" $ do
    it "String should get the same String" $ do
      env <- nullEnv
      r <- runExceptT $ eval env (String "a")
      r `shouldBe` Right (String "a")
    it "cond should return the first successive clause expression" $ do
      let code = readExpr "(cond ((> 3 2) 'greater) (#t 'less))"
      env <- primitiveBindings
      isRight code `shouldBe` True
      r <- runExceptT $ eval env (extractCode code)
      r `shouldBe` Right (Atom "greater")
  context "Effect environment" $ do
    it "let* allow second binding is done in an env in which first binding is visible" $ do
      env <- nullEnv
      r <- runExceptT $ eval env (List (Atom "let*" : List [ List [ Atom "x", Number 1 ], List [ Atom "y", Atom "x" ] ] : [Atom "y"]))
      r `shouldBe` Right (Number 1)
    it "letrec allow binding use itself recursive" $ do
      let code = readExpr "(letrec ((until (lambda (stop init) (if (> init stop) init (until stop (+ init 1)))))) (until 10 1))"
      env <- primitiveBindings
      isRight code `shouldBe` True
      r <- runExceptT $ eval env (extractCode code)
      r `shouldBe` Right (Number 11)
  where
    extractCode (Right code) = code

main :: IO ()
main = hspec spec
