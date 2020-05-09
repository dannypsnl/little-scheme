{-# LANGUAGE OverloadedStrings #-}
module Scheme.Ast.Trans (
  toStage1
  , toStage2
  , toStage3
  , toStage4
  , toCore
) where
import Control.Monad.Except
import Scheme.Ast.Core
import Scheme.Ast.Stage0
import Scheme.Ast.Stage1
import Scheme.Ast.Stage2
import Scheme.Ast.Stage3
import Scheme.Ast.Stage4

toStage1 :: Stage0 -> IOThrowsError Stage1
toStage1 (List p ((Atom _ "lambda") : (List _ (params)) : rest)) = do
  ps <- mapM parameter params
  clauses <- mapM toStage1 rest
  return $ Lambda p ps clauses
toStage1 (List p [(Atom _ "set!"), (Atom _ name), expr]) = do
  expr <- toStage1 expr
  return $ Set p name expr
toStage1 (List p [(Atom _ "if"), pred, thenE, elseE]) = do
  pred <- toStage1 pred
  e1 <- toStage1 thenE
  e2 <- toStage1 elseE
  return $ If p pred e1 e2
toStage1 (List p ((Atom _ "define") : (Atom _ name) : rest)) = do
  clauses <- mapM toStage1 rest
  return $ Define p name Nothing clauses
toStage1 (List p ((Atom _ "define") : (List _ ((Atom _ name) : params)) : rest)) = do
  ps <- mapM parameter params
  clauses <- mapM toStage1 rest
  return $ Define p name (Just ps) clauses
toStage1 (List p ((Atom _ "let") : (List _ bindings) : rest)) = do
  bs <- mapM binding bindings
  clauses <- mapM toStage1 rest
  return $ Let p bs clauses
toStage1 (List p ((Atom _ "let*") : (List _ bindings) : rest)) = do
  bs <- mapM binding bindings
  clauses <- mapM toStage1 rest
  return $ LetStar p bs clauses
toStage1 (List p ((Atom _ "letrec") : (List _ bindings) : rest)) = do
  bs <- mapM binding bindings
  clauses <- mapM toStage1 rest
  return $ LetRec p bs clauses
toStage1 (List p es) = do
  es <- mapM toStage1 es
  return $ Application p es
toStage1 stage0 = return $ Stage0 stage0

parameter :: Stage0 -> IOThrowsError Variable
parameter (Atom p v) = return $ Variable p v
parameter bad = throwError $ Default (getPosStage0 bad) (" bad parameter form " ++ show bad)

binding :: Stage0 -> IOThrowsError Stage1
binding (List p [(Atom _ name), init]) = do
  initE <- toStage1 init
  return $ Binding p name initE
binding bad = throwError $ Default (getPosStage0 bad) (" bad binding form " ++ show bad)


-- transform `let*` and `letrec` to `let`
toStage2 :: Stage1 -> IOThrowsError Stage2
toStage2 (LetStar pos bindings wrappedExpressions) = convertedToLet
  where
    convertedToLet = do
      es <- mapM toStage2 (bindings++wrappedExpressions)
      return $ foldr1 convert es
    convert bind wrappedExpr = Let_2 pos [bind] [wrappedExpr]
toStage2 (LetRec pos bindings wrappedExpressions) = do
  -- letrec can be replace by a let with pre init a temp value and set! that var later
  -- here we pre init the bindings
  parameters <- mapM preInit bindings
  -- here we create a reset bindings expressions
  setBinds <- reset bindings
  -- then evaluate a transform let
  wrappedExpressions <- mapM toStage2 wrappedExpressions
  return $ Let_2 pos parameters (setBinds ++ wrappedExpressions)
  where
    preInit :: Stage1 -> IOThrowsError Stage2
    preInit (Binding p var _) = return $ (Binding_2 p var (Stage0_2 (Quoted p (Atom p var))))
    -- we can believe that bad form already be reject by stage1
    reset :: [Stage1] -> IOThrowsError [Stage2]
    reset binds = mapM (\(Binding p var e) -> do
                                                e <- toStage2 e
                                                return $ Set_2 p var e) binds
-- copy
toStage2 (Stage0 stage0) = return $ Stage0_2 stage0
toStage2 (Lambda a b c) = do
  c <- mapM toStage2 c
  return $ Lambda_2 a b c
toStage2 (Define a b c d) = do
  d <- mapM toStage2 d
  return $ Define_2 a b c d
toStage2 (Set a b c) = do
  c <- toStage2 c
  return $ Set_2 a b c
toStage2 (Let a b c) = do
  b <- mapM toStage2 b
  c <- mapM toStage2 c
  return $ Let_2 a b c
toStage2 (Binding a b c) = do
  c <- toStage2 c
  return $ Binding_2 a b c
toStage2 (If a b c d) = do
  b <- toStage2 b
  c <- toStage2 c
  d <- toStage2 d
  return $ If_2 a b c d
toStage2 (Application a b) = do
  b <- mapM toStage2 b
  return $ Application_2 a b

-- transform `let` to `lambda`
toStage3 :: Stage2 -> IOThrowsError Stage3
toStage3 (Let_2 pos bindings body) = do
  params <- mapM paramFromBinding bindings
  args <- mapM argFromBinding bindings
  body <- mapM toStage3 body
  return $ (Application_3 pos (Lambda_3 pos params body : args))
  where
    paramFromBinding :: Stage2 -> IOThrowsError Variable
    paramFromBinding (Binding_2 pos param _) = return $ Variable pos param
    argFromBinding :: Stage2 -> IOThrowsError Stage3
    argFromBinding (Binding_2 _ _ init) = do
      arg <- toStage3 init
      return $ arg
-- copy
toStage3 (Stage0_2 stage0) = return $ Stage0_3 stage0
toStage3 (Lambda_2 a b c) = do
  c <- mapM toStage3 c
  return $ Lambda_3 a b c
toStage3 (Define_2 a b c d) = do
  d <- mapM toStage3 d
  return $ Define_3 a b c d
toStage3 (Set_2 a b c) = do
  c <- toStage3 c
  return $ Set_3 a b c
toStage3 (If_2 a b c d) = do
  b <- toStage3 b
  c <- toStage3 c
  d <- toStage3 d
  return $ If_3 a b c d
toStage3 (Application_2 a b) = do
  b <- mapM toStage3 b
  return $ Application_3 a b

-- transform `(define (name param...+) body)` to `(define name (lambda (param...+) body))`
toStage4 :: Stage3 -> IOThrowsError Stage4
toStage4 (Define_3 pos name Nothing [body]) = do
  body <- toStage4 body
  return $ Define_4 pos name body
toStage4 (Define_3 pos name Nothing body) = throwError $ Default pos "bad define form, multiple expression after identifier"
toStage4 (Define_3 pos name (Just params) body) = do
  body <- mapM toStage4 body
  return $ Define_4 pos name (Lambda_4 pos params body)
-- copy
toStage4 (Stage0_3 stage0) = return $ Stage0_4 stage0
toStage4 (Lambda_3 a b c) = do
  c <- mapM toStage4 c
  return $ Lambda_4 a b c
toStage4 (Set_3 a b c) = do
  c <- toStage4 c
  return $ Set_4 a b c
toStage4 (If_3 a b c d) = do
  b <- toStage4 b
  c <- toStage4 c
  d <- toStage4 d
  return $ If_4 a b c d
toStage4 (Application_3 a b) = do
  b <- mapM toStage4 b
  return $ Application_4 a b

toCore :: Stage4 -> IOThrowsError Core
toCore (Stage0_4 stage0) = return $ stage0ToCore stage0
  where
    stage0ToCore :: Stage0 -> Core
    stage0ToCore (Quoted pos v) = CoreQouted pos v
    stage0ToCore (Bool pos v) = CoreBool pos v
    stage0ToCore (Number pos v) = CoreNumber pos v
    stage0ToCore (String pos v) = CoreString pos v
toCore (Lambda_4 pos params body) = do
  body <- mapM toCore body
  return $ CoreLambda pos params body
toCore (Define_4 pos name expr) = do
  expr <- toCore expr
  return $ CoreDefine pos name expr
toCore (Set_4 pos name expr) = do
  expr <- toCore expr
  return $ CoreSet pos name expr
toCore (If_4 pos pred thenE elseE) = do
  pred <- toCore pred
  thenE <- toCore thenE
  elseE <- toCore elseE
  return $ CoreIf pos pred thenE elseE
toCore (Application_4 pos expressions) = do
  exprs <- mapM toCore expressions
  return $ CoreApplication pos (head exprs) (tail exprs)
