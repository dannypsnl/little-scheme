module InterpreterSpec where
import SpecHelper

import Core (ScmError(..), ScmValue(..), liftThrows, nullEnv)
import Interpreter (eval, primitiveBindings)
import Parser (readExpr)

import Control.Monad.Except (runExceptT)
import Data.Either (isLeft, isRight)

spec :: Spec
spec = describe "eval" $ do
  context "No effect to environment" $ do
    it "String" $ runCode "\"a\"" >>= (`shouldBe` Right (String "a"))
    it "Number" $ runCode "1" >>= (`shouldBe` Right (Number 1))
    it "Bool #t" $ runCode "#t" >>= (`shouldBe` Right (Bool True))
    it "Bool #f" $ runCode "#f" >>= (`shouldBe` Right (Bool False))
    it "Negtive number should work" $ runCode "((lambda (x) (cond ((< x 0) (- 0 x)) (#t x))) -1)" >>= (`shouldBe` Right (Number 1))
    it "cond should return the first successive clause expression" $
      runCode "(cond ((> 3 2) 'greater) (#t 'less))"
      >>= (`shouldBe` Right (Atom "greater"))
    it "cond would halt program when no clause success" $
      runCode "(cond (#f 'succ))"
      >>= (`shouldBe` Left (NonExhaustivePattern [List [Bool False, List [Atom "quote", Atom "succ"]]]))
    it "case should return the first successive clause expression" $
      runCode "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))"
      >>= (`shouldBe` Right (Atom "composite"))
    it "case else clause should return expression anyway" $
      runCode "(case (* 2 3) ((2 3 5 7) 'prime) (else 'composite))"
      >>= (`shouldBe` Right (Atom "composite"))
    it "case would halt program when no clause success" $
      runCode "(case 6 ((1 2) 'succ))"
      >>= (`shouldBe` Left (NonExhaustivePattern [List [List [Number 1, Number 2], List [Atom "quote", Atom "succ"]]]))
  context "Effect environment" $ do
    it "let* allow second binding is done in an env in which first binding is visible" $
      runCode "(let* ((x 1) (y x)) y)" >>= (`shouldBe` Right (Number 1))
    it "letrec allow binding use itself recursive" $
      runCode "(letrec ((until (lambda (stop init) (if (> init stop) init (until stop (+ init 1)))))) (until 10 1))"
      >>= (`shouldBe` Right (Number 11))
  where
    runCode code = do
      let c = readExpr code
      env <- primitiveBindings
      isRight c `shouldBe` True
      runExceptT $ eval env ((\(Right code) -> code) c)

main :: IO ()
main = hspec spec
