module Scheme.Ast.Stage4 (
  Stage4(..)
  , toStage4
) where
import Control.Monad.Except (throwError)
import Data.Text hiding (foldr1)
import Scheme.Ast.Stage0
import Scheme.Ast.Stage1
import Scheme.Ast.Stage3
import Scheme.Core hiding (ScmValue(..))
import Text.Megaparsec.Pos

data Stage4 = Stage0_4 Stage0
  | Lambda_4 SourcePos [Variable] [Stage4]
  | Define_4 SourcePos Text Stage4
  | Set_4 SourcePos Text Stage4
  | If_4 SourcePos Stage4 Stage4 Stage4
  | Application_4 SourcePos [Stage4]
  deriving (Show, Eq)

-- transform `(define (name param...+) body)` to `(define name (lambda (param...+) body))`
toStage4 :: Stage3 -> IOThrowsError Stage4
toStage4 (Define_3 pos name Nothing [body]) = do
  body <- toStage4 body
  return $ Define_4 pos name body
toStage4 (Define_3 pos name Nothing body) = throwError $ Default "bad define form, multiple expression after identifier"
toStage4 (Define_3 pos name (Just params) body) = do
  body <- mapM toStage4 body
  return $ Define_4 pos name (Lambda_4 pos params body)
-- copy
toStage4 (Stage0_3 stage0) = return $ Stage0_4 stage0
toStage4 (Lambda_3 a b c) = do
  c <- mapM toStage4 c
  return $ Lambda_4 a b c
toStage4 (Set_3 a b c) = do
  c <- toStage4 c
  return $ Set_4 a b c
toStage4 (If_3 a b c d) = do
  b <- toStage4 b
  c <- toStage4 c
  d <- toStage4 d
  return $ If_4 a b c d
toStage4 (Application_3 a b) = do
  b <- mapM toStage4 b
  return $ Application_4 a b
