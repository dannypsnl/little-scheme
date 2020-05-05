module Scheme.Ast.Stage2 () where
import Data.Text
import Scheme.Ast.Stage0
import Scheme.Ast.Stage1
import Scheme.Core hiding (ScmValue(..))
import Text.Megaparsec.Pos

data Stage2 = Stage0_2 Stage0
  | Lambda_2 SourcePos [Variable] [Stage2]
  | Define_2 SourcePos Text (Maybe [Variable]) [Stage2]
  | Set_2 SourcePos Text Stage2
  | Let_2 SourcePos [Stage2] [Stage2]
  | Binding_2 SourcePos Text Stage2
  | If_2 SourcePos Stage2 Stage2 Stage2
  | Application_2 SourcePos [Stage2]

-- transform `let*` and `letrec` to `let`
toStage2 :: Stage1 -> IOThrowsError Stage2
toStage2 (Stage0 stage0) = return $ Stage0_2 stage0
toStage2 (Lambda a b c) = do
  c <- mapM toStage2 c
  return $ Lambda_2 a b c
toStage2 (Define a b c d) = do
  d <- mapM toStage2 d
  return $ Define_2 a b c d
toStage2 (Set a b c) = do
  c <- toStage2 c
  return $ Set_2 a b c
toStage2 (Let a b c) = do
  b <- mapM toStage2 b
  c <- mapM toStage2 c
  return $ Let_2 a b c
toStage2 (Binding a b c) = do
  c <- toStage2 c
  return $ Binding_2 a b c
toStage2 (If a b c d) = do
  b <- toStage2 b
  c <- toStage2 c
  d <- toStage2 d
  return $ If_2 a b c d
toStage2 (Application a b) = do
  b <- mapM toStage2 b
  return $ Application_2 a b
