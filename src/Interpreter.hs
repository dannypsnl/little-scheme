{-# LANGUAGE ExistentialQuantification #-}

module Interpreter (
  ThrowsError,
  trapError,
  extractValue,
  ScmError(..),
  Env,
  eval,
  nullEnv,
  liftThrows,
  runIOThrows
) where
import Core (ScmValue(..), unwordsList)

import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Trans (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Text.Parsec.Error (ParseError)
import Text.Read (readMaybe)

type Env = IORef [(String, IORef ScmValue)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT ScmError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows act = extractValue <$> runExceptT (trapError act)

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

eval :: Env -> ScmValue -> IOThrowsError ScmValue
eval env val@(String _) = return val
eval env val@(Number _) = return val
-- `#t`, `#f`
eval env val@(Bool _) = return val
-- `a`
eval env (Atom var) = getVar env var
-- `'()`
eval env (List [Atom "quote", val]) = return val
-- `(if (= 3 3) 1 2)`
eval env (List [Atom "if", pred, left, right]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env right
    _ -> eval env left
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
-- `(+ 1 2 3)`
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)
  ]

equal :: [ScmValue] -> ThrowsError ScmValue
equal [a, b] = do
  primitiveEqual <- or <$> mapM (unpackEquals a b)
                     [AnyUnpacker unpackNumber, AnyUnpacker unpackString, AnyUnpacker unpackBool]
  eqvEquals <- eqv [a, b]
  return $ Bool (primitiveEqual || let (Bool x) = eqvEquals in x)
equal bad = throwError $ NumArgs 2 bad

data Unpacker = forall a. Eq a => AnyUnpacker (ScmValue -> ThrowsError a)

unpackEquals :: ScmValue -> ScmValue -> Unpacker -> ThrowsError Bool
unpackEquals a b (AnyUnpacker unpacker) =
  do u1 <- unpacker a
     u2 <- unpacker b
     return (u1 == u2)
  `catchError` (const $ return False)

eqv :: [ScmValue] -> ThrowsError ScmValue
eqv [Bool a, Bool b] = return $ Bool $ a == b
eqv [Number a, Number b] = return $ Bool $ a == b
eqv [String a, String b] = return $ Bool $ a == b
eqv [Atom a, Atom b] = return $ Bool $ a == b
eqv [Pair xs x, Pair ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
-- first sure length is the same
-- then check all elements are the same
eqv [List a, List b] = return $ Bool $ length a == length b && all eqvPair (zip a b)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
                             Left err -> False
                             Right (Bool v) -> v
-- different type is not equal value
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

cons :: [ScmValue] -> ThrowsError ScmValue
-- `(cons 'a '())` -> `(a)`
cons [x, List []] = return $ List [x]
-- `(cons 'a '(b c))` -> `(a b c)`
cons [x, List xs] = return $ List $ x:xs
-- `(cons 'a '(b . c))` -> `(a b . c)`
cons [x, Pair xs right] = return $ Pair (x:xs) right
-- `(cons 'a 'b)` -> `(a . b)`
cons [x, y] = return $ Pair [x] y
cons bad = throwError $ NumArgs 2 bad

car :: [ScmValue] -> ThrowsError ScmValue
-- `(car '(1 2))` -> `1`
car [List (x:xs)] = return x
-- `(car '(1 2 . 3))` -> `1`
car [Pair (x:xs) _] = return x
-- `(car 3)`, car can't work out of pair
car [bad] = throwError $ TypeMismatch "pair" bad
-- `(car 1 2)`, car only takes one argument
car badList = throwError $ NumArgs 1 badList

cdr :: [ScmValue] -> ThrowsError ScmValue
-- `(cdr '(1 2 3))` -> `(2 3)`
cdr [List (x:xs)] = return $ List xs
-- `(cdr '(2 . 3))` -> `3`
cdr [Pair [_] x] = return x
-- `(cdr '(1 2 . 3))` -> `(2 . 3)`
cdr [Pair (_:xs) x] = return $ Pair xs x
cdr [bad] = throwError $ TypeMismatch "pair" bad
cdr badList = throwError $ NumArgs 1 badList

boolBinaryOp :: (ScmValue -> ThrowsError a) -> (a -> a -> Bool) -> [ScmValue] -> ThrowsError ScmValue
boolBinaryOp unpack op [a, b] = do
  left <- unpack a
  right <- unpack b
  return $ Bool (left `op` right)
boolBinaryOp unpack _ bad = throwError $ NumArgs 2 bad

numberBoolBinaryOp = boolBinaryOp unpackNumber
stringBoolBinaryOp = boolBinaryOp unpackString
boolBoolBinaryOp = boolBinaryOp unpackBool

numberBinaryOp :: (Integer -> Integer -> Integer) -> [ScmValue] -> ThrowsError ScmValue
numberBinaryOp op           [] = throwError $ NumArgs 2 []
numberBinaryOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numberBinaryOp op params = Number . foldl1 op <$> mapM unpackNumber params

unpackString :: ScmValue -> ThrowsError String
-- Normal format, `"abcd"`
unpackString (String s) = return s
-- Number can be a string for string functions
-- e.g. `(string=? 1 "1")`
unpackString (Number s) = return $ show s
-- Bool can be a string for string functions
-- e.g. `(string=? "#t" #t)`
unpackString (Bool s) = return $ show s
unpackString notString = throwError $ TypeMismatch "string" notString

unpackBool :: ScmValue -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackNumber :: ScmValue -> ThrowsError Integer
-- Normal format `1`, `2`, `10`
unpackNumber (Number n) = return n
-- String can be a number for function takes number, e.g. `(+ 1 "2")`
unpackNumber (String n) = case readMaybe n of
  Nothing -> throwError $ TypeMismatch "number" $ String n
  Just a -> return a
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
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show ScmError where show = showError

type ThrowsError = Either ScmError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
