module Scheme.Ast.Stage4 (
  Stage4(..)
) where
import Control.Monad.Except (throwError)
import Data.Text
import Scheme.Ast.Core
import Scheme.Ast.Stage0
import Scheme.Ast.Stage1
import Scheme.Core hiding (ScmValue(..))
import Text.Megaparsec.Pos

data Stage4 = Stage0_4 Stage0
  | Lambda_4 SourcePos [Variable] [Stage4]
  | Define_4 SourcePos Text Stage4
  | Set_4 SourcePos Text Stage4
  | If_4 SourcePos Stage4 Stage4 Stage4
  | Application_4 SourcePos [Stage4]
  deriving (Show, Eq)
