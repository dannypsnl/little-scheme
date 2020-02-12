{-# LANGUAGE QuasiQuotes #-}
module TransformerSpec where
import SpecHelper

import Scheme.Parser (readExpr)
import Scheme.Transformer (desugarLet)

import Control.Monad.Except (runExceptT)

spec :: Spec
spec = context "desugarLet" $ do
  it "desugar let*" $
    "(let* ((x 1) (y x)) y)" `resultIs` "((lambda (x) ((lambda (y) y) x)) 1)"
  it "desugar let in pair" $
    "(1 . (let ((x 1)) x))" `resultIs` "(1 . ((lambda (x) x) 1))"
  it "" $
    [str|(let ((x 2) (y 3))
      (write (* x y)))
    |] `resultIs` "((lambda (x y) (write (* x y))) 2 3)"
  it "" $
    [str|(let ((x 2) (y 3))
            (let ((foo (lambda (z) (+ x y z)))
              (x 7))
          (write (foo 4))))
    |] `resultIs` "((lambda (x y) ((lambda (foo x) (write (foo 4))) (lambda (z) (+ x y z)) 7)) 2 3)"
  it "" $
    [str|(let ((x 2) (y 3))
            (let* ((x 7)
             (z (+ x y)))
        (write (* z x))))
    |] `resultIs` "((lambda (x y) ((lambda (x) ((lambda (z) (write (* z x))) (+ x y))) 7)) 2 3)"
  it "" $
    [str|(letrec (
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
    |] `resultIs` "((lambda (even? odd?) (set! even? (lambda (n) (if (zero? n) #t (odd? (- n 1))))) (set! odd? (lambda (n) (if (zero? n) #f (even? (- n 1))))) (even? 88)) (quote even?) (quote odd?))"
  where
    resultIs code expectedValue = trans code >>= (`shouldBe` readExpr expectedValue)
    trans code = do
      let c = readExpr code
      case c of
        Left err -> error (show err)
        Right code' -> runExceptT $ desugarLet code'

main :: IO ()
main = hspec spec
