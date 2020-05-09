module Scheme.Ast.Stage3 (
  Stage3(..)
) where
import Data.Text
import Scheme.Ast.Core
import Scheme.Ast.Stage0
import Scheme.Ast.Stage1
import Text.Megaparsec.Pos

data Stage3 = Stage0_3 Stage0
  | Lambda_3 SourcePos [Variable] [Stage3]
  | Define_3 SourcePos Text (Maybe [Variable]) [Stage3]
  | Set_3 SourcePos Text Stage3
  | If_3 SourcePos Stage3 Stage3 Stage3
  | Application_3 SourcePos [Stage3]
  deriving (Show, Eq)
