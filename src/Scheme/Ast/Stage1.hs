{-# LANGUAGE OverloadedStrings #-}
module Scheme.Ast.Stage1 (
  Stage1(..)
  , Variable(..)
  , toStage1
) where
import Control.Monad.Except (throwError)
import Data.Text
import Scheme.Ast.Stage0
import Scheme.Core (IOThrowsError, ScmError(..))
import Text.Megaparsec.Pos

data Variable = Variable SourcePos Text
  deriving (Show, Eq)
data Stage1 =
  -- reuse
  Stage0 Stage0
  -- extract structure from s-expression
  -- (lambda parameter... clause...)
  | Lambda SourcePos [Variable] [Stage1]
  -- ;;; variable form
  -- (define a 1)
  -- ;;; function forms
  -- (define (a) a)
  -- (define (a b c)
  --   (set! b 1)
  --   c)
  ---------------------------------------------------
  -- Define <pos> <name> <maybe<param+>> <clause+>
  ---------------------------------------------------
  | Define SourcePos Text (Maybe [Variable]) [Stage1]
  -- (let ([a 1]
  --       [b 2])
  --   a)
  | Let SourcePos [Stage1] [Stage1]
  | LetStar SourcePos [Stage1] [Stage1]
  | LetRec SourcePos [Stage1] [Stage1]
  -- TODO: if, cond, case, <application>
  | Binding SourcePos Text Stage1
  deriving (Show, Eq)

toStage1 :: Stage0 -> IOThrowsError Stage1
toStage1 (List p ((Atom _ "lambda") : (List _ (params)) : rest)) = do
  ps <- mapM parameter params
  clauses <- mapM toStage1 rest
  return $ Lambda p ps clauses
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
toStage1 (List p bad) = throwError $ Default $ (show p) ++ " unknown form " ++ show bad
toStage1 stage0 = return $ Stage0 stage0

parameter :: Stage0 -> IOThrowsError Variable
parameter (Atom p v) = return $ Variable p v
parameter bad = throwError $ Default $ " bad parameter form " ++ show bad

binding :: Stage0 -> IOThrowsError Stage1
binding (List p [(Atom _ name), init]) = do
  initE <- toStage1 init
  return $ Binding p name initE
binding bad = throwError $ Default $ " bad binding form " ++ show bad
