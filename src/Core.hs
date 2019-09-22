module Core (
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

instance Show ScmValue where
  show = showValue

showValue :: ScmValue -> String
showValue (String str) = "\"" ++ str ++ "\""
showValue (Atom name) = name
showValue (Number num) = show num
showValue (Bool True) = "#t"
showValue (Bool False) = "#f"
showValue (List contents) = "(" ++ unwordsList contents ++ ")"
showValue (Pair head tail) = "(" ++ unwordsList head ++ " . " ++ showValue tail ++ ")"
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
  NumArgs Integer [ScmValue]
  | TypeMismatch String ScmValue
  | ParserErr ParseError
  | BadSpecialForm String ScmValue
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: ScmError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (ParserErr parseErr) = "Parse error at " ++ show parseErr

instance Show ScmError where show = showError

type IOThrowsError = ExceptT ScmError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

type ThrowsError = Either ScmError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
