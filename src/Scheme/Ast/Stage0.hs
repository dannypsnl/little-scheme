module Scheme.Ast.Stage0 (
  Stage0(..)
  , getPosStage0
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

getPosStage0 :: Stage0 -> SourcePos
getPosStage0 (Quoted pos _) = pos
getPosStage0 (List pos _) = pos
getPosStage0 (Bool pos _) = pos
getPosStage0 (Atom pos _) = pos
getPosStage0 (Number pos _) = pos
getPosStage0 (String pos _) = pos
