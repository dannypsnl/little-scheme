module InterpreterSpec where
import SpecHelper

import Core (ScmValue(..), liftThrows, nullEnv)
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
  context "Effect environment" $ do
    it "let* allow second binding is done in an env in which first binding is visible" $ do
      env <- nullEnv
      r <- runExceptT $ eval env (List (Atom "let*" : List [ List [ Atom "x", Number 1 ], List [ Atom "y", Atom "x" ] ] : [Atom "y"]))
      r `shouldBe` Right (Number 1)
    it "letrec allow binding use itself recursive" $ do
      code <- return $ readExpr "(letrec ((until (lambda (stop init) (if (> init stop) init (until stop (+ init 1)))))) (until 10 1))"
      env <- primitiveBindings
      isRight code `shouldBe` True
      r <- runExceptT $ eval env ((\(Right extractCode) -> extractCode) code)
      r `shouldBe` Right (Number 11)

main :: IO ()
main = hspec spec
