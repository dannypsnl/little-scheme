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
  | Lambda SourcePos [String] [Stage1] -- (lambda parameter... clause...)
  | Define SourcePos
  | Let SourcePos [Stage1] [Stage1]
  deriving (Show, Eq)

fromStage0 :: Stage0 -> IOThrowsError Stage1
fromStage0 (List p ((Atom _ "lambda") : rest)) = return $ Lambda p [] []
fromStage0 (List p ((Atom _ "define") : rest)) = return $ Define p
fromStage0 (List p ((Atom _ "let") : rest)) = return $ Let p [] []
fromStage0 (List p bad) = throwError $ Default $ "unknown form" ++ show bad
fromStage0 stage0 = return $ Stage0 stage0
