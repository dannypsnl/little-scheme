module Scheme.Ast.Stage0 (
  Stage0(..)
) where
import Data.Text
import Text.Megaparsec.Pos

data Stage0 =
  Quoted SourcePos Stage0
  | List SourcePos [Stage0]
  -- value
  | Bool SourcePos Bool
  | Atom SourcePos Text
  | Number SourcePos Integer
  | String SourcePos Text
  -- TODO: pair
  deriving (Show, Eq)
