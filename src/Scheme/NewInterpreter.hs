module Scheme.NewInterpreter () where
import Control.Monad.Except
import Control.Monad.Trans
import Data.Maybe
import Data.Text hiding (drop, last, length, map, zip)
import Scheme.Ast.Core
import Text.Megaparsec.Pos

-- eval
evalCore :: Env -> Core -> IOThrowsError Core
evalCore env v@(CoreNumber _ _) = return v
evalCore env v@(CoreBool _ _) = return v
evalCore env v@(CoreString _ _) = return v
evalCore env (CoreDefine pos name expr) = do
  expr <- evalCore env expr
  r <- defineVar env pos name expr
  return $ r
evalCore env (CoreSet pos name expr) = do
  expr <- evalCore env expr
  r <- setVar env pos name expr
  return $ r
evalCore env (CoreIf pos pred thenE elseE) = do
  result <- evalCore env pred
  case result of
    CoreBool _ True -> evalCore env thenE
    _ -> evalCore env elseE
evalCore env (CoreApplication pos f args) = do
  f' <- evalCore env f
  args <- mapM (evalCore env) args
  apply f' args

-- produce runtime function
makeFunc :: Monad m => SourcePos -> Maybe Text -> Env -> [Core] -> [Core] -> m Core
-- FIXME: correct params
makeFunc pos varargs env params body = return $ CoreRuntime pos (Func [] varargs body env)
makeNormalFunc :: Env -> SourcePos -> [Core] -> [Core] -> IOThrowsError Core
makeNormalFunc env pos = makeFunc pos Nothing env

-- application
apply :: Core -> [Core] -> IOThrowsError Core
apply (CoreRuntime pos (PrimitiveFunc f)) args = liftThrows $ f args
apply (CoreRuntime pos (Func params vararg body closure)) args =
  if length params /= length args && isNothing vararg
    then throwError $ NumArgs pos (toInteger $ length params) args
    else do
      -- bind params with args
      mapM (\(var, val) -> defineVar closure pos var val) (zip params args)
      evalBody closure body
    where
      evalBody env body = last <$> mapM (evalCore env) body
apply (CoreRuntime pos (IOFunc f)) args = f args
apply bad _ = throwError $  NotFunction (getPos bad) "Applying a not function value" bad
