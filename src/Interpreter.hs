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
import Text.Read (readMaybe)

eval :: ScmValue -> ThrowsError ScmValue
eval val@(String _)             = return val
eval val@(Number _)             = return val
-- `#t`
-- `#f`
eval val@(Bool _)               = return val
-- `'()`
eval (List [Atom "quote", val]) = return val
-- `(if (= 3 3) 1 2)`
eval (List [Atom "if", pred, left, right]) = do
  result <- eval pred
  case result of
    Bool False -> eval right
    _          -> eval left
-- `(+ 1 2 3)`
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [ScmValue] -> ThrowsError ScmValue
apply func args =
  maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

primitives :: [(String, [ScmValue] -> ThrowsError ScmValue)]
primitives = [
  ("+", numberBinaryOp (+))
  , ("-", numberBinaryOp (-))
  , ("*", numberBinaryOp (*))
  , ("/", numberBinaryOp div)
  , ("mod", numberBinaryOp mod)
  , ("quotient", numberBinaryOp quot)
  , ("remainder", numberBinaryOp rem)
  , ("=", numberBoolBinaryOp (==))
  , ("<", numberBoolBinaryOp (<))
  , (">", numberBoolBinaryOp (>))
  , ("/=", numberBoolBinaryOp (/=))
  , (">=", numberBoolBinaryOp (>=))
  , ("<=", numberBoolBinaryOp (<=))
  , ("&&", boolBoolBinaryOp (&&))
  , ("||", boolBoolBinaryOp (||))
  , ("string=?", stringBoolBinaryOp (==))
  , ("string<?", stringBoolBinaryOp (<))
  , ("string>?", stringBoolBinaryOp (>))
  , ("string<=?", stringBoolBinaryOp (<=))
  , ("string>=?", stringBoolBinaryOp (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  ]

cons :: [ScmValue] -> ThrowsError ScmValue
-- `(cons 'a '())` -> `(a)`
cons [x, List []]       = return $ List [x]
-- `(cons 'a '(b c))` -> `(a b c)`
cons [x, List xs]       = return $ List $ x:xs
-- `(cons 'a '(b . c))` -> `(a b . c)`
cons [x, Pair xs right] = return $ Pair (x:xs) right
-- `(cons 'a 'b)` -> `(a . b)`
cons [x, y]             = return $ Pair [x] y
cons bad                = throwError $ NumArgs 2 bad

car :: [ScmValue] -> ThrowsError ScmValue
-- `(car '(1 2))` -> `1`
car [List (x:xs)]   = return x
-- `(car '(1 2 . 3))` -> `1`
car [Pair (x:xs) _] = return x
-- `(car 3)`, car can't work out of pair
car [bad]           = throwError $ TypeMismatch "pair" bad
-- `(car 1 2)`, car only takes one argument
car badList         = throwError $ NumArgs 1 badList

cdr :: [ScmValue] -> ThrowsError ScmValue
-- `(cdr '(1 2 3))` -> `(2 3)`
cdr [List (x:xs)]   = return $ List xs
-- `(cdr '(2 . 3))` -> `3`
cdr [Pair [_] x]    = return x
-- `(cdr '(1 2 . 3))` -> `(2 . 3)`
cdr [Pair (_:xs) x] = return $ Pair xs x
cdr [bad]           = throwError $ TypeMismatch "pair" bad
cdr badList         = throwError $ NumArgs 1 badList

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
-- Normal format, `"abcd"`
unpackString (String s) = return s
-- Number can be a string for string functions
-- e.g. `(string=? 1 "1")`
unpackString (Number s) = return $ show s
-- Bool can be a string for string functions
-- e.g. `(string=? "#t" #t)`
unpackString (Bool s)   = return $ show s
unpackString notString  = throwError $ TypeMismatch "string" notString

unpackBool :: ScmValue -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackNumber :: ScmValue -> ThrowsError Integer
-- Normal format `1`, `2`, `10`
unpackNumber (Number n) = return n
-- String can be a number for function takes number, e.g. `(+ 1 "2")`
unpackNumber (String n) = case readMaybe n of
  Nothing -> throwError $ TypeMismatch "number" $ String n
  Just a  -> return a
-- When list only contains one element then we can use it as number
-- e.g. `(+ 1 '(2))`
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
