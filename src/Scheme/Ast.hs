module Scheme.Ast(
  ScmAst(..)
) where
import Data.Text
import Text.Megaparsec.Pos

data ScmAst =
  Lambda
  | Define
  | Let
  -- value
  | Quoted SourcePos ScmAst
  | List SourcePos [ScmAst]
  | Bool SourcePos Bool
  | Atom SourcePos Text
  | Number SourcePos Integer
  | String SourcePos Text
