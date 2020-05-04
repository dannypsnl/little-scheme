module Scheme.Ast(
  ScmAst(..)
  , ScmExpr(..)
) where
import Data.Text
import Text.Megaparsec.Pos

data ScmAst = ScmAst {
  pos :: SourcePos
  , sExpr :: ScmExpr }

data ScmExpr =
  Lambda
  | Define
  | Let
  | Bool Bool
  | Atom Text
  | Number Integer
