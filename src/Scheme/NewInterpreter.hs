{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Scheme.NewInterpreter () where
import Control.Monad.Except
import Control.Monad.Trans
import Data.Maybe
import Data.Text hiding (cons, drop, foldl1, last, length, map, zip)
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
-- FIXME: maybe try to convert core qouted to list?
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

primitives :: [(String, SourcePos -> [Core] -> ThrowsError Core)]
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

equal :: SourcePos -> [Core] -> ThrowsError Core
equal pos [a, b] = do
  primitiveEqual <- or <$> mapM (unpackEquals a b)
                     [AnyUnpacker unpackNumber, AnyUnpacker unpackString, AnyUnpacker unpackBool]
  eqvEquals <- eqv pos [a, b]
  return $ CoreBool pos (primitiveEqual || let (CoreBool pos x) = eqvEquals in x)
equal pos bad = throwError $ NumArgs pos 2 bad

data Unpacker = forall a. Eq a => AnyUnpacker (Core -> ThrowsError a)

unpackEquals :: Core -> Core -> Unpacker -> ThrowsError Bool
unpackEquals a b (AnyUnpacker unpacker) =
  do u1 <- unpacker a
     u2 <- unpacker b
     return (u1 == u2)
  `catchError` const (return False)

eqv :: SourcePos -> [Core] -> ThrowsError Core
eqv pos [CoreBool _ a, CoreBool _ b] = return $ CoreBool pos (a == b)
eqv pos [CoreNumber _ a, CoreNumber _ b] = return $ CoreBool pos (a == b)
eqv pos [CoreString _ a, CoreString _ b] = return $ CoreBool pos (a == b)
-- FIXME: handle Pair and List
-- eqv pos [Pair xs x, Pair ys y] = eqv pos [List $ xs ++ [x], List $ ys ++ [y]]
-- -- first sure length is the same
-- -- then check all elements are the same
-- eqv pos [List a, List b] = return $ Bool $ length a == length b && all eqvPair (zip a b)
--   where
--     eqvPair :: (Core, Core) -> Bool
--     eqvPair (x1, x2) = case eqv [x1, x2] of
--                              Left _ -> False
--                              Right (Bool v) -> v
--                              _ -> False
-- different type is not equal value
eqv pos [_, _] = return $ CoreBool pos False
eqv pos badArgList = throwError $ NumArgs pos 2 badArgList

cons :: SourcePos -> [Core] -> ThrowsError Core
-- FIXME: handle Pair and List
-- `(cons 'a '())` -> `(a)`
-- cons pos [x, List []] = return $ List [x]
-- `(cons 'a '(b c))` -> `(a b c)`
-- cons pos [x, List xs] = return $ List $ x:xs
-- `(cons 'a '(b . c))` -> `(a b . c)`
-- cons pos [x, Pair xs right] = return $ Pair (x:xs) right
-- `(cons 'a 'b)` -> `(a . b)`
-- cons pos [x, y] = return $ Pair [x] y
cons pos bad = throwError $ NumArgs pos 2 bad

car :: SourcePos -> [Core] -> ThrowsError Core
-- FIXME: handle Pair and List
-- `(car '(1 2))` -> `1`
-- car pos [List (x:_)] = return x
-- `(car '(1 2 . 3))` -> `1`
-- car pos [Pair (x:_) _] = return x
-- `(car 3)`, car can't work out of pair
car pos [bad] = throwError $ TypeMismatch pos "pair" bad
-- `(car 1 2)`, car only takes one argument
car pos badList = throwError $ NumArgs pos 1 badList

cdr :: SourcePos -> [Core] -> ThrowsError Core
-- FIXME: handle Pair and List
-- `(cdr '(1 2 3))` -> `(2 3)`
-- cdr pos [List (_:xs)] = return $ List xs
-- `(cdr '(2 . 3))` -> `3`
-- cdr pos [Pair [_] x] = return x
-- `(cdr '(1 2 . 3))` -> `(2 . 3)`
-- cdr pos [Pair (_:xs) x] = return $ Pair xs x
cdr pos [bad] = throwError $ TypeMismatch pos "pair" bad
cdr pos badList = throwError $ NumArgs pos 1 badList

boolBinaryOp :: (Core -> ThrowsError a) -> (a -> a -> Bool) -> SourcePos -> [Core] -> ThrowsError Core
boolBinaryOp unpack op pos [a, b] = do
  left <- unpack a
  right <- unpack b
  return $ CoreBool pos (left `op` right)
boolBinaryOp _ _ pos bad = throwError $ NumArgs pos 2 bad

numberBoolBinaryOp :: (Integer -> Integer -> Bool) -> SourcePos -> [Core] -> ThrowsError Core
numberBoolBinaryOp op pos = boolBinaryOp unpackNumber op pos
stringBoolBinaryOp :: (String -> String -> Bool) -> SourcePos -> [Core] -> ThrowsError Core
stringBoolBinaryOp op pos = boolBinaryOp unpackString op pos
boolBoolBinaryOp :: (Bool -> Bool -> Bool) -> SourcePos -> [Core] -> ThrowsError Core
boolBoolBinaryOp op pos = boolBinaryOp unpackBool op pos

numberBinaryOp :: (Integer -> Integer -> Integer) -> SourcePos -> [Core] -> ThrowsError Core
numberBinaryOp _op pos [] = throwError $ NumArgs pos 2 []
numberBinaryOp _op pos singleVal@[_] = throwError $ NumArgs pos 2 singleVal
numberBinaryOp op pos parameters = do
  ps <- mapM unpackNumber parameters
  return $ CoreNumber pos (foldl1 op ps)

unpackString :: Core -> ThrowsError String
-- Normal format, `"abcd"`
unpackString (CoreString _ s) = return (T.unpack s)
unpackString notString = throwError $ TypeMismatch (getPos notString) "string" notString

unpackBool :: Core -> ThrowsError Bool
unpackBool (CoreBool _ b) = return b
unpackBool notBool = throwError $ TypeMismatch (getPos notBool) "boolean" notBool

unpackNumber :: Core -> ThrowsError Integer
-- Normal format `1`, `2`, `10`
unpackNumber (CoreNumber _ n) = return n
unpackNumber notNum = throwError $ TypeMismatch (getPos notNum) "number" notNum
