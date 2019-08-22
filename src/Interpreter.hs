module Interpreter (
  eval
) where
import Parser (ScmValue(..))

eval :: ScmValue -> ScmValue
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args))  = apply func $ map eval args

apply :: String -> [ScmValue] -> ScmValue
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [ScmValue] -> ScmValue)]
primitives = [
  ("+", numberBinaryOp (+)),
  ("-", numberBinaryOp (-)),
  ("*", numberBinaryOp (*)),
  ("/", numberBinaryOp div),
  ("mod", numberBinaryOp mod),
  ("quotient", numberBinaryOp quot),
  ("remainder", numberBinaryOp rem)]

numberBinaryOp :: (Integer -> Integer -> Integer) -> [ScmValue] -> ScmValue
numberBinaryOp op params = Number $ foldl1 op $ map unpackNumber params

unpackNumber :: ScmValue -> Integer
unpackNumber (Number n) = n
unpackNumber (String n) =
  let
    parsed = reads n :: [(Integer, String)]
  in
    if null parsed
      then 0
      else fst $ head parsed
unpackNumber (List [n]) = unpackNumber n
unpackNumber _ = 0
