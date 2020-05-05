{-# LANGUAGE OverloadedStrings #-}
module Scheme.Ast.Stage1 (
  Stage1(..)
  , fromStage0
) where
import Control.Monad.Except (throwError)
import Data.Text
import Scheme.Ast.Stage0
import Scheme.Core (IOThrowsError, ScmError(..))
import Text.Megaparsec.Pos

data Stage1 =
  -- reuse
  Stage0 Stage0
  -- extract structure from s-expression
  -- (lambda parameter... clause...)
  | Lambda SourcePos [Text] [Stage1]
  -- ;;; variable form
  -- (define a 1)
  -- ;;; function forms
  -- (define (a) a)
  -- (define (a b c)
  --   (set! b 1)
  --   c)
  ------------------------------------------------
  -- Define <pos> <name> <maybe<param+>> <clause+>
  ------------------------------------------------
  | Define SourcePos Text (Maybe [Text]) [Stage1]
  -- (let ([a 1]
  --       [b 2])
  --   a)
  | Let SourcePos [Stage1] [Stage1]
  | Binding SourcePos Text Stage1
  deriving (Show, Eq)

fromStage0 :: Stage0 -> IOThrowsError Stage1
fromStage0 (List p ((Atom _ "lambda") : (List _ (params)) : rest)) = do
  ps <- mapM parameter params
  clauses <- mapM fromStage0 rest
  return $ Lambda p ps clauses
fromStage0 (List p ((Atom _ "define") : (Atom _ name) : rest)) = do
  clauses <- mapM fromStage0 rest
  return $ Define p name Nothing clauses
fromStage0 (List p ((Atom _ "define") : (List _ ((Atom _ name) : params)) : rest)) = do
  ps <- mapM parameter params
  clauses <- mapM fromStage0 rest
  return $ Define p name (Just ps) clauses
fromStage0 (List p ((Atom _ "let") : (List _ bindings) : rest)) = do
  bs <- mapM binding bindings
  clauses <- mapM fromStage0 rest
  return $ Let p bs clauses
fromStage0 (List p bad) = throwError $ Default $ (show p) ++ " unknown form " ++ show bad
fromStage0 stage0 = return $ Stage0 stage0

parameter :: Stage0 -> IOThrowsError Text
parameter (Atom _ v) = return v
parameter bad = throwError $ Default $ " bad parameter form " ++ show bad

binding :: Stage0 -> IOThrowsError Stage1
binding (List p [(Atom _ name), init]) = do
  initE <- fromStage0 init
  return $ Binding p name initE
binding bad = throwError $ Default $ " bad binding form " ++ show bad
