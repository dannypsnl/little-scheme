{-# LANGUAGE OverloadedStrings #-}
module Scheme.NewInterpreter () where
import Control.Monad.Except
import Control.Monad.Trans
import Data.Maybe
import Data.Text hiding (drop, last, length, map, zip)
import qualified Data.Text as T
import Data.Text.IO
import Prelude hiding (readFile)
import Scheme.Ast.Core
import Scheme.Ast.Trans
import Scheme.NewParser
import System.IO (IOMode(..), hClose, hPrint, openFile, stdin, stdout)
import Text.Megaparsec
import Text.Megaparsec.Error
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

-- primitives
ioPrimitives :: [(Text, SourcePos -> [Core] -> IOThrowsError Core)]
ioPrimitives = [
  ("apply", applyProc)
  , ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("read-contents", readContents)
  ]

applyProc :: SourcePos -> [Core] -> IOThrowsError Core
-- (apply f '(1 2 3))
applyProc pos [func, CoreQouted pos1 arg] = do
  args' <- toStage1 arg >>= toStage2 >>= toStage3 >>= toStage4 >>= toCore
  apply func [args']
-- (apply f 1 2 3)
applyProc pos (func : args) = apply func args
applyProc pos bad = throwError $ Default pos ("expected a function and a list of arguments or a list with a function as head but got: " ++ show bad)

makePort :: IOMode -> SourcePos -> [Core] -> IOThrowsError Core
makePort mode pos [CoreString _ filename] = fmap (\port -> CoreRuntime pos (Port port)) $ liftIO $ openFile (T.unpack filename) mode
makePort _ pos [bad] = throwError $ TypeMismatch pos "string" bad
makePort _ pos bad = throwError $ NumArgs pos 1 bad

closePort :: SourcePos -> [Core] -> IOThrowsError Core
closePort pos [CoreRuntime _ (Port port)] = liftIO (hClose port) >> return (CoreBool pos True)
closePort pos _ = return $ CoreBool pos False

readProc :: SourcePos -> [Core] -> IOThrowsError Core
readProc pos [] = readProc pos [CoreRuntime pos (Port stdin)]
readProc pos [CoreRuntime _ (Port port)] = do
   input <- liftIO (hGetLine port)
   r <- case parse expr (T.unpack "scheme") input of
     Left err -> throwError $ Default pos (errorBundlePretty err)
     Right val -> return val
   toStage1 r >>= toStage2 >>= toStage3 >>= toStage4 >>= toCore
readProc pos bad = throwError $ Default pos ("expected an object and a optional port(default port is stdin) but got: " ++ show bad)

writeProc :: SourcePos -> [Core] -> IOThrowsError Core
writeProc pos [obj] = writeProc pos [obj, CoreRuntime pos (Port stdout)]
writeProc pos [obj, CoreRuntime _ (Port port)] = liftIO (hPrint port obj) >> return (CoreBool pos True)
writeProc pos bad = throwError $ Default pos ("expected an object and a optional port(default port is stdout) but got: " ++ show bad)

readContents :: SourcePos -> [Core] -> IOThrowsError Core
readContents pos [CoreString _ filename] = fmap (CoreString pos) $ liftIO $ readFile (T.unpack filename)
readContents pos [bad] = throwError $ TypeMismatch pos "string" bad
readContents pos bad = throwError $ NumArgs pos 1 bad
