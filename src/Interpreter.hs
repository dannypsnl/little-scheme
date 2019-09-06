module Interpreter (
  ThrowsError,
  trapError,
  extractValue,
  ScmError(..),
  eval
) where
import Parser (ScmValue(..), unwordsList)

import Control.Monad.Except (catchError, throwError)
import Text.Parsec.Error (ParseError)

eval :: ScmValue -> ThrowsError ScmValue
eval val@(String _)             = return val
eval val@(Number _)             = return val
-- #t
-- #f
eval val@(Bool _)               = return val
-- '()
eval (List [Atom "quote", val]) = return val
-- (+ 1 2 3)
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [ScmValue] -> ThrowsError ScmValue
apply func args =
  maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

primitives :: [(String, [ScmValue] -> ThrowsError ScmValue)]
primitives = [
  ("+", numberBinaryOp (+)),
  ("-", numberBinaryOp (-)),
  ("*", numberBinaryOp (*)),
  ("/", numberBinaryOp div),
  ("mod", numberBinaryOp mod),
  ("quotient", numberBinaryOp quot),
  ("remainder", numberBinaryOp rem),
  ("=", numberBoolBinaryOp (==)),
  ("<", numberBoolBinaryOp (<)),
  (">", numberBoolBinaryOp (>)),
  ("/=", numberBoolBinaryOp   (/=)),
  (">=", numberBoolBinaryOp   (>=)),
  ("<=", numberBoolBinaryOp   (<=)),
  ("&&", boolBoolBinaryOp   (&&)),
  ("||", boolBoolBinaryOp   (||)),
  ("string=?", stringBoolBinaryOp   (==)),
  ("string<?", stringBoolBinaryOp   (<)),
  ("string>?", stringBoolBinaryOp   (>)),
  ("string<=?", stringBoolBinaryOp   (<=)),
  ("string>=?", stringBoolBinaryOp   (>=))
  ]

boolBinaryOp :: (ScmValue -> ThrowsError a) -> (a -> a -> Bool) -> [ScmValue] -> ThrowsError ScmValue
boolBinaryOp unpack op args =
  if length args /= 2
  then throwError $ NumArgs 2 args
  else do
    left <- unpack $ head args
    right <- unpack $ args !! 1
    return $ Bool $ left `op` right

numberBoolBinaryOp  = boolBinaryOp unpackNumber
stringBoolBinaryOp  = boolBinaryOp unpackString
boolBoolBinaryOp = boolBinaryOp unpackBool

numberBinaryOp :: (Integer -> Integer -> Integer) -> [ScmValue] -> ThrowsError ScmValue
numberBinaryOp op           []  = throwError $ NumArgs 2 []
numberBinaryOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numberBinaryOp op params        = Number . foldl1 op <$> mapM unpackNumber params

unpackString :: ScmValue -> ThrowsError String
unpackString (String s) = return s
unpackString (Number s) = return $ show s
unpackString (Bool s)   = return $ show s
unpackString notString  = throwError $ TypeMismatch "string" notString

unpackBool :: ScmValue -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackNumber :: ScmValue -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber (String n) =
  let parsed = reads n in
    if null parsed
      then throwError $ TypeMismatch "number" $ String n
      else return $ fst $ head parsed
unpackNumber (List [n]) = unpackNumber n
unpackNumber notNum = throwError $ TypeMismatch "number" notNum

data ScmError =
  NumArgs Integer [ScmValue]
  | TypeMismatch String ScmValue
  | Parser ParseError
  | BadSpecialForm String ScmValue
  | NotFunction String String
  | UnboundVar String String
  | Default String


showError :: ScmError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show ScmError where show = showError

type ThrowsError = Either ScmError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
