module Scheme.Interpreter.Transformer (
  desugarLet
) where
import Scheme.Core (IOThrowsError, ScmError(..), ScmValue(..))

import Control.Monad.Except (throwError)

desugarLet :: ScmValue -> IOThrowsError ScmValue
desugarLet (List (Atom "let" : List bindings : expressions)) = do
  -- because `(let ((x 1)) x)` is equal to `((lambda (x) (x)) 1)`
  -- we use lambda to implement let
  -- create a lambda
  parameters <- mapM takeParam bindings
  let func = List (Atom "lambda" : List parameters : expressions)
  -- take inits as the argument of lambda
  args <- mapM takeInit bindings
  -- apply lambda
  desugarLet (List (func : args))
  where
    takeParam :: ScmValue -> IOThrowsError ScmValue
    takeParam (List [Atom var, _]) = return (Atom var)
    takeParam bad = throwError $ BadSpecialForm "Expect a binding `(var, init)` but got" bad
    -- @takeInit can believe that bad form already be reject by @takeParam
    takeInit :: ScmValue -> IOThrowsError ScmValue
    takeInit (List [_, initExpr]) = return initExpr
    -- This condition is unlikely happened, but to ensure at future when we move it to others place it still work
    -- we have to complete the pattern
    takeInit bad = throwError $ BadSpecialForm "Expect a binding `(var, init)` but got" bad
-- stand for the bad form such as: `(let 1 'expressions)`
desugarLet (List (Atom "let" : bad : _wrapped)) = throwError $ BadSpecialForm "Expect a list of bindings but got" bad
desugarLet (List (Atom "let*" : List bindings : wrappedExpressions)) = desugarLet convertedToLet
  where
    convertedToLet = foldr1 convert (bindings++wrappedExpressions)
    convert bind wrappedExpr = List [Atom "let", List [bind], wrappedExpr]
desugarLet (List (Atom "letrec" : List bindings : wrappedExpressions)) = do
  -- letrec can be replace by a let with pre init a temp value and set! that var later
  -- here we pre init the bindings
  parameters <- mapM preInit bindings
  -- here we create a reset bindings expressions
  setBinds <- reset bindings
  -- then evaluate a transform let
  desugarLet (List (Atom "let" : List parameters : (setBinds ++ wrappedExpressions)))
  where
    preInit :: ScmValue -> IOThrowsError ScmValue
    preInit (List [Atom var, _]) = return (List [Atom var, List [Atom "quote", Atom var]])
    preInit bad = throwError $ BadSpecialForm "Expect a binding but got" bad
    -- @reset can believe that bad form already be reject by @preInit
    reset :: [ScmValue] -> IOThrowsError [ScmValue]
    reset binds = return $ map (\(List b) -> List (Atom "set!" : b)) binds
-- ensure quoted value won't be converted
desugarLet form@(List (Atom "quote" : _)) = return form
-- nest expression also have to convert
-- we skip runtime ScmValue since parser won't generate them
desugarLet (List l) = List <$> mapM desugarLet l
desugarLet (Pair l r) = do
  left <- mapM desugarLet l
  right <- desugarLet r
  return $ Pair left right
desugarLet form = return form
