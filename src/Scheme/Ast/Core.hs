{-# LANGUAGE RecordWildCards #-}
module Scheme.Ast.Core (
  Core(..)
  , getPos
  , toCore
  , Runtime(..)
  , Env
  , EnvImpl(..)
  , setVar, getVar, defineVar
  , ScmError(..)
  , IOThrowsError, ThrowsError
  , liftThrows
) where
import Control.Monad.Except
import Control.Monad.Trans
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Scheme.Ast.Stage0
import Scheme.Ast.Stage1
import Scheme.Ast.Stage4
import System.IO (Handle)
import Text.Megaparsec.Pos

data Core = CoreNumber SourcePos Integer
  | CoreBool SourcePos Bool
  | CoreString SourcePos Text
  | CoreQouted SourcePos Stage0
  | CoreLambda SourcePos [Variable] [Core]
  | CoreDefine SourcePos Text Core
  | CoreSet SourcePos Text Core
  | CoreIf SourcePos Core Core Core
  | CoreApplication SourcePos Core [Core]
  -- runtime
  | CoreRuntime SourcePos Runtime

getPos :: Core -> SourcePos
getPos (CoreNumber pos _) = pos
getPos (CoreBool pos _) = pos
getPos (CoreString pos _) = pos
getPos (CoreQouted pos _) = pos
getPos (CoreLambda pos _ _) = pos
getPos (CoreDefine pos _ _) = pos
getPos (CoreSet pos _ _) = pos
getPos (CoreIf pos _ _ _) = pos
getPos (CoreApplication pos _ _) = pos
getPos (CoreRuntime pos _) = pos

data Runtime = PrimitiveFunc ([Core] -> ThrowsError Core)
  | Func [Text] (Maybe Text) [Core] Env
  | IOFunc ([Core] -> IOThrowsError Core)
  | Port Handle

toCore :: Stage4 -> IOThrowsError Core
toCore (Stage0_4 stage0) = return $ stage0ToCore stage0
  where
    stage0ToCore :: Stage0 -> Core
    stage0ToCore (Quoted pos v) = CoreQouted pos v
    stage0ToCore (Bool pos v) = CoreBool pos v
    stage0ToCore (Number pos v) = CoreNumber pos v
    stage0ToCore (String pos v) = CoreString pos v
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

-- Env
data EnvImpl = EnvImpl {
  env :: Map Text (IORef Core)
  , parentEnv :: Maybe Env }
type Env = IORef EnvImpl

getVar :: Env -> SourcePos -> Text -> IOThrowsError Core
getVar envRef pos var = do
  EnvImpl { env=env, parentEnv=parentEnv } <- liftIO $ readIORef envRef
  case parentEnv of
    Nothing ->
      maybe (throwError $ UnboundVar pos "Can not found variable" (T.unpack var))
        (liftIO . readIORef)
        (Map.lookup var env)
    Just parent -> getVar parent pos var

setVar :: Env -> SourcePos -> Text -> Core -> IOThrowsError Core
setVar envRef pos var val = do
  EnvImpl { env=env, parentEnv=parentEnv } <- liftIO $ readIORef envRef
  case parentEnv of
    Nothing ->
      maybe (throwError $ UnboundVar pos "Can not set an unbound variable" (T.unpack var))
        (liftIO . readIORef)
        (Map.lookup var env)
    Just parent -> setVar parent pos var val

defineVar :: Env -> SourcePos -> Text -> Core -> IOThrowsError Core
defineVar envRef pos var val = do
  isDefined <- liftIO $ isBound envRef var
  if isDefined
    then setVar envRef pos var val >> return val
    else liftIO $ do
      valueRef <- newIORef val
      env'@EnvImpl{env=env, ..} <- readIORef envRef
      writeIORef envRef env' { env=(Map.insert var valueRef env) }
      return val
  where
    isBound :: Env -> Text -> IO Bool
    isBound env var = do
      EnvImpl { env=env, .. } <- readIORef env
      return $ isJust (Map.lookup var env)

-- show position
showPos :: SourcePos -> String
showPos (SourcePos {sourceName=sourceName, sourceLine=sourceLine, sourceColumn=sourceColumn}) =
  sourceName ++ ":" ++ (show sourceLine) ++ ":" ++ (show sourceColumn) ++ ":"

-- show core
instance Eq Core where
  (==) l r = show l == show r
  (/=) l r = show l /= show r

instance Show Core where
  show = showValue

showValue :: Core -> String
showValue (CoreString _ str) = "\"" ++ (T.unpack str) ++ "\""
showValue (CoreNumber _ num) = show num
showValue (CoreBool _ True) = "#t"
showValue (CoreBool _ False) = "#f"
showValue (CoreRuntime _ (PrimitiveFunc _)) = "<primitive>"
showValue (CoreRuntime _  (Func args varargs _ _)) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ (T.unpack arg)) ++ ") ...)"
showValue (CoreRuntime _ (Port _)) = "<IO port>"
showValue (CoreRuntime _ (IOFunc _)) = "<IO primitive>"

unwordsList :: [Core] -> String
unwordsList = unwords . map showValue
-- error
data ScmError =
  -- NumArgs expected-number-of-arguments list-of-parameters-actual-got
  NumArgs SourcePos Integer [Core]
  -- TypeMismatch type-string-format value-actual-got
  --
  -- example:
  --   TypeMismatch "string" bad
  | TypeMismatch SourcePos String Core
  | BadSpecialForm SourcePos String Core
  | NotFunction SourcePos String Core
  | UnboundVar SourcePos String String
  | Default SourcePos String
  | NonExhaustivePattern SourcePos [Core]


instance Eq ScmError where
  (==) l r = show l == show r
  (/=) l r = show l /= show r

showError :: ScmError -> String
showError (UnboundVar pos message varname) = (showPos pos) ++ message ++ ": " ++ varname
showError (BadSpecialForm pos message form) = (showPos pos) ++ message ++ ": " ++ show form
showError (NotFunction pos message func) = (showPos pos) ++ message ++ ": " ++ show func
showError (NumArgs pos expected found) = (showPos pos) ++ "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch pos expected found) = (showPos pos) ++ "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (NonExhaustivePattern pos clauses) = (showPos pos) ++ "Non-exhaustive patterns: " ++ unwordsList clauses
showError (Default pos message) = (showPos pos) ++ "Error: " ++ message

instance Show ScmError where show = showError

type IOThrowsError = ExceptT ScmError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

type ThrowsError = Either ScmError
