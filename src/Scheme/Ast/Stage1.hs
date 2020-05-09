{-# LANGUAGE OverloadedStrings #-}
module Scheme.Ast.Stage1 (
  Stage1(..)
  , Variable(..)
) where
import Control.Monad.Except (throwError)
import Data.Text
import Scheme.Ast.Stage0
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
  -- (set! x 1)
  ---------------------------
  -- Set <pos> <name> <expr>
  ---------------------------
  | Set SourcePos Text Stage1
  -- (let ([a 1]
  --       [b 2])
  --   a)
  | Let SourcePos [Stage1] [Stage1]
  | LetStar SourcePos [Stage1] [Stage1]
  | LetRec SourcePos [Stage1] [Stage1]
  | Binding SourcePos Text Stage1
  -- (if prediction
  --   thenE
  --   elseE)
  -----------------------------------
  -- if <prediction> <thenE> <elseE>
  -----------------------------------
  | If SourcePos Stage1 Stage1 Stage1
  | Application SourcePos [Stage1]
  -- TODO: cond, case
  deriving (Show, Eq)
