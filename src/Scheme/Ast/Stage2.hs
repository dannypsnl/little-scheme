module Scheme.Ast.Stage2 (
  Stage2(..)
  , toStage2
) where
import Data.Text hiding (foldr1)
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
  deriving (Show, Eq)

-- transform `let*` and `letrec` to `let`
toStage2 :: Stage1 -> IOThrowsError Stage2
toStage2 (LetStar pos bindings wrappedExpressions) = convertedToLet
  where
    convertedToLet = do
      es <- mapM toStage2 (bindings++wrappedExpressions)
      return $ foldr1 convert es
    convert bind wrappedExpr = Let_2 pos [bind] [wrappedExpr]
toStage2 (LetRec pos bindings wrappedExpressions) = do
  -- letrec can be replace by a let with pre init a temp value and set! that var later
  -- here we pre init the bindings
  parameters <- mapM preInit bindings
  -- here we create a reset bindings expressions
  setBinds <- reset bindings
  -- then evaluate a transform let
  wrappedExpressions <- mapM toStage2 wrappedExpressions
  return $ Let_2 pos parameters (setBinds ++ wrappedExpressions)
  where
    preInit :: Stage1 -> IOThrowsError Stage2
    preInit (Binding p var _) = return $ (Binding_2 p var (Stage0_2 (Quoted p (Atom p var))))
    -- we can believe that bad form already be reject by stage1
    reset :: [Stage1] -> IOThrowsError [Stage2]
    reset binds = mapM (\(Binding p var e) -> do
                                                e <- toStage2 e
                                                return $ Set_2 p var e) binds
-- copy
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
