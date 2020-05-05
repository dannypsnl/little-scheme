module Scheme.Ast(
  ScmAst(..)
) where
import Data.Text
import Text.Megaparsec.Pos

data ScmAst =
  Lambda
  | Define
  | Let
  | Bool SourcePos Bool
  | Atom SourcePos Text
  | Number SourcePos Integer
