module Scheme.Ast.Stage3 (
  Stage3(..)
  , toStage3
) where
import Data.Text
import Scheme.Ast.Stage0
import Scheme.Ast.Stage1
import Scheme.Ast.Stage2
import Scheme.Core hiding (ScmValue(..))
import Text.Megaparsec.Pos

data Stage3 = Stage0_3 Stage0
  | Lambda_3 SourcePos [Variable] [Stage3]
  | Define_3 SourcePos Text (Maybe [Variable]) [Stage3]
  | Set_3 SourcePos Text Stage3
  | If_3 SourcePos Stage3 Stage3 Stage3
  | Application_3 SourcePos [Stage3]
  deriving (Show, Eq)

-- transform `let` to `lambda`
toStage3 :: Stage2 -> IOThrowsError Stage3
toStage3 (Let_2 pos bindings body) = do
  params <- mapM paramFromBinding bindings
  args <- mapM argFromBinding bindings
  body <- mapM toStage3 body
  return $ (Application_3 pos (Lambda_3 pos params body : args))
  where
    paramFromBinding :: Stage2 -> IOThrowsError Variable
    paramFromBinding (Binding_2 pos param _) = return $ Variable pos param
    argFromBinding :: Stage2 -> IOThrowsError Stage3
    argFromBinding (Binding_2 _ _ init) = do
      arg <- toStage3 init
      return $ arg
-- copy
toStage3 (Stage0_2 stage0) = return $ Stage0_3 stage0
toStage3 (Lambda_2 a b c) = do
  c <- mapM toStage3 c
  return $ Lambda_3 a b c
toStage3 (Define_2 a b c d) = do
  d <- mapM toStage3 d
  return $ Define_3 a b c d
toStage3 (Set_2 a b c) = do
  c <- toStage3 c
  return $ Set_3 a b c
toStage3 (If_2 a b c d) = do
  b <- toStage3 b
  c <- toStage3 c
  d <- toStage3 d
  return $ If_3 a b c d
toStage3 (Application_2 a b) = do
  b <- mapM toStage3 b
  return $ Application_3 a b