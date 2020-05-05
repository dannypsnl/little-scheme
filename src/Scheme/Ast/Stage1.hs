module Scheme.Ast.Stage1 (
  Stage1(..)
) where
import Data.Text
import Text.Megaparsec.Pos

data Stage1 =
  Lambda SourcePos [String] [Stage1] -- (lambda parameter... clause...)
  | Define SourcePos
  | Let SourcePos [Stage1] [Stage1]
  -- value
  | Bool SourcePos Bool
  | Atom SourcePos Text
  | Number SourcePos Integer
  | String SourcePos Text
