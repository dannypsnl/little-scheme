module Scheme.Ast.Core () where
import Data.Text hiding (head, tail)
import Scheme.Ast.Stage0
import Scheme.Ast.Stage1
import Scheme.Ast.Stage4
import Scheme.Core hiding (ScmValue(..))
import Text.Megaparsec.Pos

data Core = CoreNumber SourcePos Integer
  | CoreBool SourcePos Bool
  | CoreString SourcePos Text
  | CoreQouted SourcePos [Stage0]
  | CoreLambda SourcePos [Variable] [Core]
  | CoreDefine SourcePos Text Core
  | CoreSet SourcePos Text Core
  | CoreIf SourcePos Core Core Core
  | CoreApplication SourcePos Core [Core]

toCore :: Stage4 -> IOThrowsError Core
toCore (Lambda_4 pos params body) = do
  body <- mapM toCore body
  return $ CoreLambda pos params body
toCore (Define_4 pos name expr) = do
  expr <- toCore expr
  return $ CoreDefine pos name expr
toCore (Set_4 pos name expr) = do
  expr <- toCore expr
  return $ CoreSet pos name expr
toCore (If_4 pos pred thenE elseE) = do
  pred <- toCore pred
  thenE <- toCore thenE
  elseE <- toCore elseE
  return $ CoreIf pos pred thenE elseE
toCore (Application_4 pos expressions) = do
  exprs <- mapM toCore expressions
  return $ CoreApplication pos (head exprs) (tail exprs)
