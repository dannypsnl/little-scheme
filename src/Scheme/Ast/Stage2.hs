module Scheme.Ast.Stage2 (
  Stage2(..)
) where
import Data.Text hiding (foldr1)
import Scheme.Ast.Stage0
import Scheme.Ast.Stage1
import Text.Megaparsec.Pos

data Stage2 = Stage0_2 Stage0
  | Lambda_2 SourcePos [Variable] [Stage2]
  | Define_2 SourcePos Text (Maybe [Variable]) [Stage2]
  | Set_2 SourcePos Text Stage2
  | Let_2 SourcePos [Stage2] [Stage2]
  | Binding_2 SourcePos Text Stage2
  | If_2 SourcePos Stage2 Stage2 Stage2
  | Application_2 SourcePos [Stage2]
  deriving (Show, Eq)
