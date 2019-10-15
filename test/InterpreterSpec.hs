{-# LANGUAGE QuasiQuotes #-}
module InterpreterSpec where
import SpecHelper

import Scheme.Core (ScmError(..), ScmValue(..))
import Scheme.Interpreter (eval', primitiveBindings)
import Scheme.Parser (readExpr)

import Control.Monad.Except (runExceptT)

spec :: Spec
spec = describe "eval" $ do
  context "No effect to environment" $ do
    it "String" $ "\"a\"" `resultIs` String "a"
    it "Number" $ "1" `resultIs` Number 1
    it "Bool #t" $ "#t" `resultIs` Bool True
    it "Bool #f" $ "#f" `resultIs` Bool False
    it "Negtive number should work" $
      [str|((lambda (x)
               (cond
                 ((< x 0) (- 0 x))
                 (#t x)))
           -1)
      |] `resultIs` Number 1
    it "cond should return the first successive clause expression" $
      [str|(cond
             [(> 3 2) 'greater]
             [#t 'less])
      |] `resultIs` Atom "greater"
    it "cond would halt program when no clause success" $
      runCode "(cond (#f 'succ))"
      >>= (`shouldBe` Left (NonExhaustivePattern [List [Bool False, List [Atom "quote", Atom "succ"]]]))
    it "case should return the first successive clause expression" $
      [str|(case (* 2 3)
             ((2 3 5 7) 'prime)
             ((1 4 6 8 9) 'composite))
      |] `resultIs` Atom "composite"
    it "case else clause should return expression anyway" $
      [str|(case (* 2 3)
             ((2 3 5 7) 'prime)
             (else 'composite))
      |] `resultIs` Atom "composite"
    it "case would halt program when no clause success" $
      runCode "(case 6 ((1 2) 'succ))"
      >>= (`shouldBe` Left (NonExhaustivePattern [List [List [Number 1, Number 2], List [Atom "quote", Atom "succ"]]]))
    it "clause should be able to have several expressions" $
      [str|(case (* 2 3)
             ((2 3 5 7) 'prime)
             (else (define x 1) 'composite))
      |] `resultIs` Atom "composite"
  context "Effect environment" $ do
    it "let* allow second binding is done in an env in which first binding is visible" $
      [str|(let* ((x 1)
                  (y x))
            y)
      |] `resultIs` Number 1
    it "letrec allow binding use itself recursive" $
      [str|(letrec ([until (lambda (stop init)
             (if (> init stop)
               init
               [until stop (+ init 1)]))])
           (until 10 1))
      |] `resultIs` Number 11
    it "" $
      [str|(let ((x 2) (y 3))
        (* x y))
      |] `resultIs` Number 6
    it "" $
      [str|(let ((x 2) (y 3))
              (let ((foo (lambda (z) (+ x y z)))
                (x 7))
            (foo 4)))
      |] `resultIs` Number 9
    it "" $
      [str|(let ((x 2) (y 3))
              (let* ((x 7)
               (z (+ x y)))
          (* z x)))
      |] `resultIs` Number 70
    it "" $
      [str|(letrec (
         (zero? (lambda (n) (= 0 n)))
      (even?
        (lambda (n)
          (if (zero? n)
            #t
            (odd? (- n 1)))))
      (odd?
        (lambda (n)
          (if (zero? n)
            #f
            (even? (- n 1)))))
    )
      (even? 88))
      |] `resultIs` Bool True
  where
    resultIs code expectedValue = runCode code >>= (`shouldBe` Right expectedValue)
    runCode code = do
      let c = readExpr code
      env <- primitiveBindings
      case c of
        Left err -> error (show err)
        Right code' -> runExceptT $ eval' env code'

main :: IO ()
main = hspec spec
