{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}

module Scheme.Interpreter (
  eval,
  runIOThrows,
  bindVars,
  primitiveBindings
) where
import Scheme.Core (Env, IOThrowsError, ScmError(..), ScmValue(..), ThrowsError, bindVars, defineVar, getVar, liftThrows, nullEnv, setVar, showValue)
import Scheme.Interpreter.Transformer (convertToCore, desugarLet)
import Scheme.Meta (defaultLibraryPath)
import Scheme.Parser (readExpr, readExprList)

import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Maybe (fromMaybe, isNothing)
import System.Directory (findFile)
import System.IO (IOMode(ReadMode, WriteMode), hClose, hGetLine, hPrint, openFile, stdin, stdout)
import Text.Read (readMaybe)

runIOThrows :: IOThrowsError String -> IO String
runIOThrows act = do
  result <- runExceptT (trapError act)
  case result of
    Right val -> return val
    Left err -> return $ show err
  where
    trapError action = catchError action (return . show)

load :: String -> IOThrowsError [ScmValue]
load filename = liftIO (readFileWithDefaultPath filename) >>= liftThrows . readExprList

readFileWithDefaultPath :: FilePath -> IO String
readFileWithDefaultPath filename = do
  libPath <- defaultLibraryPath
  file <- findFile [libPath] filename
  readFile (fromMaybe filename file)

eval :: Env -> ScmValue -> IOThrowsError ScmValue
eval env (List [Atom "load", String filename]) = load filename >>= fmap last . mapM (eval env)
eval env val = desugarLet val >>= convertToCore >>= evalCore env

evalCore :: Env -> ScmValue -> IOThrowsError ScmValue
evalCore _env val@(String _) = return val
evalCore _env val@(Number _) = return val
-- `#t`, `#f`
evalCore _env val@(Bool _) = return val
-- `a`
evalCore env (Atom var) = getVar env var
-- `'()`
evalCore _env (List [Atom "quote", val]) = return val
-- `(if (= 3 3) 1 2)`
evalCore env (List [Atom "if", prediction, left, right]) = do
  result <- evalCore env prediction
  case result of
    Bool False -> evalCore env right
    _ -> evalCore env left
-- ```
-- (cond
--   ((> x 0) 'positive)
--   (#t 'negative))
-- ;;; 'positive
-- (cond ((> 3 2) 'greater)
--   ((< 3 2) 'less))
-- ;;; 'greater
-- ```
evalCore env (List (Atom "cond" : clauses)) = range clauses
  where
    range [] = throwError $ NonExhaustivePattern clauses
    range [List (Atom "else" : expr)] = last <$> mapM (evalCore env) expr
    range (List (prediction : expr) : rest) = do
      result <- evalCore env prediction
      case result of
        Bool True -> last <$> mapM (evalCore env) expr
        _ -> range rest
    range bad = throwError $ TypeMismatch "clause" (List bad)
evalCore env (List (Atom "case" : key : clauses)) = range clauses
  where
    range [] = throwError $ NonExhaustivePattern clauses
    range [List (Atom "else" : expr)] = last <$> mapM (evalCore env) expr
    range (List (List objects : expr) : rest) = do
      matched <- rangeObjects objects
      case matched of
        Bool True -> last <$> mapM (evalCore env) expr
        _ -> range rest
    range bad = throwError $ TypeMismatch "clause" (List bad)
    rangeObjects [] = return $ Bool False
    rangeObjects (object:rest) = do
      matched <- evalCore env (List (Atom "eqv?" : [object, key]))
      case matched of
        Bool True -> return $ Bool True
        _ -> rangeObjects rest
evalCore env (List [Atom "set!", Atom var, form]) = evalCore env form >>= setVar env var
-- `(define x 1)`
evalCore env (List [Atom "define", Atom var, form]) = evalCore env form >>= defineVar env var
-- `(define (f x y) (+ x y))`
evalCore env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
-- `(define (f x y . a) (+ x y a))`
evalCore env (List (Atom "define" : Pair (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
-- `(lambda (x y) (+ x y))`
evalCore env (Lambda params varargs body) =
  makeFunc varargs env params body
-- `(+ 1 2 3)`
evalCore env (List (function : args)) = do
  -- get function value
  func <- evalCore env function
  argVals <- mapM (evalCore env) args
  apply func argVals
evalCore _env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc :: Monad m => Maybe String -> Env -> [ScmValue] -> [ScmValue] -> m ScmValue
makeFunc varargs env params body = return $ Func (map showValue params) varargs body env
makeNormalFunc :: Env -> [ScmValue] -> [ScmValue] -> ExceptT ScmError IO ScmValue
makeNormalFunc = makeFunc Nothing
makeVarArgs :: ScmValue -> Env -> [ScmValue] -> [ScmValue] -> ExceptT ScmError IO ScmValue
makeVarArgs = makeFunc . Just . showValue

apply :: ScmValue -> [ScmValue] -> IOThrowsError ScmValue
apply (PrimitiveFunc f) args = liftThrows $ f args
apply Func {params, vararg, body, closure} args =
  if length params /= length args && isNothing vararg
    then throwError $ NumArgs (toInteger $ length params) args
    else liftIO (bindVars closure $ zip params args) >>= bindVarArgs vararg >>= evalBody
    where
      evalBody env = last <$> mapM (evalCore env) body
      bindVarArgs arg env = case arg of
        Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
        Nothing -> return env
      remainingArgs = drop (length params) args
apply (IOFunc f) args = f args
apply bad _ = throwError $ NotFunction "Applying a not function value" bad

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (`bindVars` (ioPs ++ ps))
  where
    ioPs = map (makeFn IOFunc) ioPrimitives
    ps = map (makeFn PrimitiveFunc) primitives
    makeFn funcType (var, func) = (var, funcType func)

ioPrimitives :: [(String, [ScmValue] -> IOThrowsError ScmValue)]
ioPrimitives = [
  ("apply", applyProc)
  , ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("read-contents", readContents)
  , ("read-all", readAll)
  ]

applyProc :: [ScmValue] -> IOThrowsError ScmValue
-- (apply f '(1 2 3))
applyProc [func, List args] = apply func args
-- (apply f 1 2 3)
applyProc (func : args) = apply func args
applyProc bad = throwError $ Default ("expected a function and a list of arguments or a list with a function as head but got: " ++ show bad)

makePort :: IOMode -> [ScmValue] -> IOThrowsError ScmValue
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode
makePort _mode [bad] = throwError $ TypeMismatch "string" bad
makePort _mode bad = throwError $ NumArgs 1 bad

closePort :: [ScmValue] -> IOThrowsError ScmValue
closePort [Port port] = liftIO (hClose port) >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [ScmValue] -> IOThrowsError ScmValue
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc bad = throwError $ Default ("expected an object and a optional port(default port is stdin) but got: " ++ show bad)

writeProc :: [ScmValue] -> IOThrowsError ScmValue
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO (hPrint port obj) >> return (Bool True)
writeProc bad = throwError $ Default ("expected an object and a optional port(default port is stdout) but got: " ++ show bad)

readContents :: [ScmValue] -> IOThrowsError ScmValue
readContents [String filename] = fmap String $ liftIO $ readFile filename
readContents [bad] = throwError $ TypeMismatch "string" bad
readContents bad = throwError $ NumArgs 1 bad

readAll :: [ScmValue] -> IOThrowsError ScmValue
readAll [String filename] = List <$> load filename
readAll [bad] = throwError $ TypeMismatch "string" bad
readAll bad = throwError $ NumArgs 1 bad

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
  `catchError` const (return False)

eqv :: [ScmValue] -> ThrowsError ScmValue
eqv [Bool a, Bool b] = return $ Bool $ a == b
eqv [Number a, Number b] = return $ Bool $ a == b
eqv [String a, String b] = return $ Bool $ a == b
eqv [Atom a, Atom b] = return $ Bool $ a == b
eqv [Pair xs x, Pair ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
-- first sure length is the same
-- then check all elements are the same
eqv [List a, List b] = return $ Bool $ length a == length b && all eqvPair (zip a b)
  where
    eqvPair :: (ScmValue, ScmValue) -> Bool
    eqvPair (x1, x2) = case eqv [x1, x2] of
                             Left _ -> False
                             Right (Bool v) -> v
                             _ -> False
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
car [List (x:_)] = return x
-- `(car '(1 2 . 3))` -> `1`
car [Pair (x:_) _] = return x
-- `(car 3)`, car can't work out of pair
car [bad] = throwError $ TypeMismatch "pair" bad
-- `(car 1 2)`, car only takes one argument
car badList = throwError $ NumArgs 1 badList

cdr :: [ScmValue] -> ThrowsError ScmValue
-- `(cdr '(1 2 3))` -> `(2 3)`
cdr [List (_:xs)] = return $ List xs
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
boolBinaryOp _unpack _ bad = throwError $ NumArgs 2 bad

numberBoolBinaryOp :: (Integer -> Integer -> Bool) -> [ScmValue] -> ThrowsError ScmValue
numberBoolBinaryOp = boolBinaryOp unpackNumber
stringBoolBinaryOp :: (String -> String -> Bool) -> [ScmValue] -> ThrowsError ScmValue
stringBoolBinaryOp = boolBinaryOp unpackString
boolBoolBinaryOp :: (Bool -> Bool -> Bool) -> [ScmValue] -> ThrowsError ScmValue
boolBoolBinaryOp = boolBinaryOp unpackBool

numberBinaryOp :: (Integer -> Integer -> Integer) -> [ScmValue] -> ThrowsError ScmValue
numberBinaryOp _op           [] = throwError $ NumArgs 2 []
numberBinaryOp _op singleVal@[_] = throwError $ NumArgs 2 singleVal
numberBinaryOp op parameters = Number . foldl1 op <$> mapM unpackNumber parameters

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
