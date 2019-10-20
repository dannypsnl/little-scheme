module Scheme.Core (
  Env
  , nullEnv
  , ScmValue(..)
  , ScmError(..)
  , ThrowsError
  , liftThrows
  , IOThrowsError
  , unwordsList
  , showValue
  , isBound
  , getVar
  , setVar
  , defineVar
  , bindVars
) where
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import System.IO (Handle)
import Text.Parsec.Error (ParseError)

type Env = IORef [(String, IORef ScmValue)]

isBound :: Env -> String -> IO Bool
isBound env var = isJust . lookup var <$> readIORef env

getVar :: Env -> String -> IOThrowsError ScmValue
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Can not found variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> ScmValue -> IOThrowsError ScmValue
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Can not set an unbound variable" var)
        (liftIO . (`writeIORef` val))
        (lookup var env)
  return val

defineVar :: Env -> String -> ScmValue -> IOThrowsError ScmValue
defineVar envRef var val = do
  isDefined <- liftIO $ isBound envRef var
  if isDefined
    then setVar envRef var val >> return val
    else liftIO $ do
      valueRef <- newIORef val
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return val

bindVars :: Env -> [(String, ScmValue)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv >>= newIORef
  where
    extendEnv env = fmap (++ env) (mapM addBinding bindings)
    addBinding (var, val) = do
      ref <- newIORef val
      return (var, ref)

nullEnv :: IO Env
nullEnv = newIORef []

data ScmValue =
  Atom String
  | List [ScmValue]
  | Pair [ScmValue] ScmValue
  | Number Integer
  | String String
  | Bool Bool
  -- core lanuage(reduce List pattern)
  --
  -- Lambda parameters vararg body
  | Lambda [ScmValue] (Maybe String) [ScmValue]
  -- If prediction then else
  | If ScmValue ScmValue ScmValue
  -- runtime only
  | PrimitiveFunc ([ScmValue] -> ThrowsError ScmValue)
  | Func { params :: [String]
           , vararg :: Maybe String
           , body :: [ScmValue]
           , closure :: Env }
  | IOFunc ([ScmValue] -> IOThrowsError ScmValue)
  | Port Handle

instance Eq ScmValue where
  (==) l r = show l == show r
  (/=) l r = show l /= show r

instance Show ScmValue where
  show = showValue

showValue :: ScmValue -> String
showValue (String str) = "\"" ++ str ++ "\""
showValue (Atom name) = name
showValue (Number num) = show num
showValue (Bool True) = "#t"
showValue (Bool False) = "#f"
showValue (List contents) = "(" ++ unwordsList contents ++ ")"
showValue (Pair pHead pTail) = "(" ++ unwordsList pHead ++ " . " ++ showValue pTail ++ ")"
showValue (PrimitiveFunc _) = "<primitive>"
showValue Func {params = args, vararg = varargs} =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showValue (Port _) = "<IO port>"
showValue (IOFunc _) = "<IO primitive>"

unwordsList :: [ScmValue] -> String
unwordsList = unwords . map showValue

data ScmError =
  -- NumArgs expected-number-of-arguments list-of-parameters-actual-got
  NumArgs Integer [ScmValue]
  -- TypeMismatch type-string-format value-actual-got
  --
  -- example:
  --   TypeMismatch "string" bad
  | TypeMismatch String ScmValue
  | ParserErr ParseError
  | BadSpecialForm String ScmValue
  | NotFunction String ScmValue
  | UnboundVar String String
  | Default String
  | NonExhaustivePattern [ScmValue]

instance Eq ScmError where
  (==) l r = show l == show r
  (/=) l r = show l /= show r

showError :: ScmError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (ParserErr parseErr) = "Parse error at " ++ show parseErr
showError (NonExhaustivePattern clauses) = "Non-exhaustive patterns: " ++ unwordsList clauses
showError (Default message) = "Error: " ++ message

instance Show ScmError where show = showError

type IOThrowsError = ExceptT ScmError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

type ThrowsError = Either ScmError
