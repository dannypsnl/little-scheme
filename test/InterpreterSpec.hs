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
    it "String" $ runCode "\"a\"" >>= (`shouldBe` Right (String "a"))
    it "Number" $ runCode "1" >>= (`shouldBe` Right (Number 1))
    it "Bool #t" $ runCode "#t" >>= (`shouldBe` Right (Bool True))
    it "Bool #f" $ runCode "#f" >>= (`shouldBe` Right (Bool False))
    it "cond should return the first successive clause expression" $
      runCode "(cond ((> 3 2) 'greater) (#t 'less))" >>= (`shouldBe` Right (Atom "greater"))
  context "Effect environment" $ do
    it "let* allow second binding is done in an env in which first binding is visible" $
      runCode "(let* ((x 1) (y x)) y)" >>= (`shouldBe` Right (Number 1))
    it "letrec allow binding use itself recursive" $
      runCode "(letrec ((until (lambda (stop init) (if (> init stop) init (until stop (+ init 1)))))) (until 10 1))" >>= (`shouldBe` Right (Number 11))
  where
    runCode code = do
      let c = readExpr code
      env <- primitiveBindings
      isRight c `shouldBe` True
      runExceptT $ eval env ((\(Right code) -> code) c)

main :: IO ()
main = hspec spec
