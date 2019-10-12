{-# LANGUAGE ExistentialQuantification #-}

module Scheme.Interpreter (
  eval,
  runIOThrows,
  bindVars,
  primitiveBindings
) where
import Scheme.Core (Env, IOThrowsError, ScmError(..), ScmValue(..), ThrowsError, bindVars, defineVar, getVar, liftThrows, nullEnv, setVar, showValue, trapError)
import Scheme.Meta (defaultLibraryPath)
import Scheme.Parser (readExpr, readExprList)

import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Maybe (fromMaybe, isNothing)
import System.Directory (findFile)
import System.FilePath (FilePath)
import System.IO (IOMode(ReadMode, WriteMode), hClose, hGetLine, hPrint, openFile, stdin, stdout)
import Text.Read (readMaybe)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows act = extractValue <$> runExceptT (trapError act)

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
-- ```
-- (cond
--   ((> x 0) 'positive)
--   (#t 'negative))
-- ;;; 'positive
-- (cond ((> 3 2) 'greater)
--   ((< 3 2) 'less))
-- ;;; 'greater
-- ```
eval env (List (Atom "cond" : clauses)) = range clauses
  where
    range [] = throwError $ NonExhaustivePattern clauses
    range (List (pred : expr) : rest) = do
      result <- eval env pred
      case result of
        Bool True -> last <$> mapM (eval env) expr
        _ -> range rest
eval env (List (Atom "case" : key : clauses)) = range clauses
  where
    range [] = throwError $ NonExhaustivePattern clauses
    range [List (Atom "else" : expr)] = last <$> mapM (eval env) expr
    range (List (List objects : expr) : rest) = do
      result <- eval env key
      matched <- rangeObjects objects
      case matched of
        Bool True -> last <$> mapM (eval env) expr
        _ -> range rest
    rangeObjects [] = return $ Bool False
    rangeObjects (object:rest) = do
      matched <- (eval env (List (Atom "eqv?" : [object, key])))
      case matched of
        Bool True -> return $ Bool True
        _ -> rangeObjects rest
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
-- `(define x 1)`
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
-- `(define (f x y) (+ x y))`
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
-- `(define (f x y . a) (+ x y a))`
eval env (List (Atom "define" : Pair (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
-- `(lambda (x y) (+ x y))`
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : Pair params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List (Atom "let" : List bindings : body)) = do
  -- because `(let ((x 1)) x)` is equal to `((lambda (x) (x)) 1)`
  -- we use lambda to implement let
  -- create a lambda
  params <- mapM takeParam bindings
  let func = List (Atom "lambda" : List params : body)
  -- take inits as the argument of lambda
  args <- mapM takeInit bindings
  -- apply lambda
  eval env (List (func : args))
  where
    takeParam :: ScmValue -> IOThrowsError ScmValue
    takeParam (List [Atom var, _]) = return (Atom var)
    takeParam bad = throwError $ BadSpecialForm "Expect a binding `(var, init)` but got" bad
    -- @takeInit can believe that bad form already be reject by @takeParam
    takeInit :: ScmValue -> IOThrowsError ScmValue
    takeInit (List [_, init]) = return init
-- stand for the bad form such as: `(let 1 'body)`
eval env (List (Atom "let" : bad : _restBody)) = throwError $ BadSpecialForm "Expect a list of bindings but got" bad
eval env (List (Atom "let*" : List bindings : body)) = eval env convertedToLet
  where
    convertedToLet = foldr1 convert (bindings++body)
    convert bind body = List [Atom "let", List [bind], body]
eval env (List (Atom "letrec" : List bindings : body)) = do
  -- letrec can be replace by a let with pre init a temp value and set! that var later
  -- here we pre init the bindings
  params <- mapM preInit bindings
  -- here we create a reset bindings expressions
  setBinds <- reset bindings
  -- then evaluate a transform let
  eval env (List (Atom "let" : List params : (setBinds ++ body)))
  where
    preInit :: ScmValue -> IOThrowsError ScmValue
    preInit (List [Atom var, _]) = return (List [Atom var, List [Atom "quote", Atom var]])
    preInit bad = throwError $ BadSpecialForm "Expect a binding but got" bad
    -- @reset can believe that bad form already be reject by @preInit
    reset :: [ScmValue] -> IOThrowsError [ScmValue]
    reset bindings = return $ map (\(List b) -> List (Atom "set!" : b)) bindings
eval env (List [Atom "load", String filename]) =
  load filename >>= fmap last . mapM (eval env)
-- `(+ 1 2 3)`
eval env (List (function : args)) = do
  -- get function value
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc varargs env params body = return $ Func (map showValue params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs :: ScmValue -> Env -> [ScmValue] -> [ScmValue] -> ExceptT ScmError IO ScmValue
makeVarArgs = makeFunc . Just . showValue

apply :: ScmValue -> [ScmValue] -> IOThrowsError ScmValue
apply (PrimitiveFunc f) args = liftThrows $ f args
apply (Func params varargs body closure) args =
  if length params /= length args && isNothing varargs
    then throwError $ NumArgs (toInteger $ length params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where
      evalBody env = last <$> mapM (eval env) body
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
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

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

writeProc :: [ScmValue] -> IOThrowsError ScmValue
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO (hPrint port obj) >> return (Bool True)

readContents :: [ScmValue] -> IOThrowsError ScmValue
readContents [String filename] = fmap String $ liftIO $ readFile filename
readContents [bad] = throwError $ TypeMismatch "string" bad
readContents bad = throwError $ NumArgs 1 bad

load :: String -> IOThrowsError [ScmValue]
load filename = liftIO (readFileWithDefaultPath filename) >>= liftThrows . readExprList

readFileWithDefaultPath :: FilePath -> IO String
readFileWithDefaultPath filename = do
  libPath <- defaultLibraryPath
  file <- findFile [libPath] filename
  readFile (fromMaybe filename file)

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
