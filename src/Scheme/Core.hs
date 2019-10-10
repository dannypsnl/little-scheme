module Scheme.Core (
  Env,
  nullEnv,
  ScmValue(..),
  ScmError(..),
  ThrowsError,
  extractValue,
  liftThrows,
  IOThrowsError,
  unwordsList,
  trapError,
  showValue
) where
import Control.Monad.Except (ExceptT, catchError, throwError)
import Data.IORef (IORef, newIORef)
import System.IO (Handle)
import Text.Parsec.Error (ParseError)

type Env = IORef [(String, IORef ScmValue)]

nullEnv :: IO Env
nullEnv = newIORef []

data ScmValue =
  Atom String
  | List [ScmValue]
  | Pair [ScmValue] ScmValue
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([ScmValue] -> ThrowsError ScmValue)
  | Func { params :: [String]
           , vararg :: Maybe String
           , body :: [ScmValue]
           , closure :: Env }
  | IOFunc ([ScmValue] -> IOThrowsError ScmValue)
  | Port Handle

instance Eq ScmValue where
  (==) l r = (show l) == (show r)
  (/=) l r = (show l) /= (show r)

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
  | NotFunction String String
  | UnboundVar String String
  | Default String
  | NonExhaustivePattern [ScmValue]

instance Eq ScmError where
  (==) l r = (show l) == (show r)
  (/=) l r = (show l) /= (show r)

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

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
