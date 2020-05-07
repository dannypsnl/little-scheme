module Scheme.NewInterpreter () where
import Control.Monad.Except
import Control.Monad.Trans
import Data.IORef
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text
import qualified Data.Text as T
import Scheme.Ast.Core
import Scheme.Core (IOThrowsError, ScmError(..))

-- eval
evalCore :: Env -> Core -> IOThrowsError Core
evalCore env v@(CoreNumber _ _) = return v
evalCore env v@(CoreBool _ _) = return v
evalCore env v@(CoreString _ _) = return v
evalCore env (CoreDefine pos name expr) = do
  expr <- evalCore env expr
  r <- defineVar env name expr
  return $ r
evalCore env (CoreSet pos name expr) = do
  expr <- evalCore env expr
  r <- setVar env name expr
  return $ r

-- Env
type Env = IORef (Map Text (IORef Core))

setVar :: Env -> Text -> Core -> IOThrowsError Core
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Can not set an unbound variable" (T.unpack var))
        (liftIO . (`writeIORef` val))
        (Map.lookup var env)
  return val

defineVar :: Env -> Text -> Core -> IOThrowsError Core
defineVar envRef var val = do
  isDefined <- liftIO $ isBound envRef var
  if isDefined
    then setVar envRef var val >> return val
    else liftIO $ do
      valueRef <- newIORef val
      env <- readIORef envRef
      writeIORef envRef (Map.insert var valueRef env)
      return val
  where
    isBound :: Env -> Text -> IO Bool
    isBound env var = isJust . Map.lookup var <$> readIORef env
