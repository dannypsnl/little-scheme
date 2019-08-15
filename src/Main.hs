module Main where
import Parser

import qualified System.Environment as Env
import qualified Text.ParserCombinators.Parsec as Parsec

main :: IO ()
main = do
  (expr:_) <- Env.getArgs
  putStrLn (readExpr expr)

readExpr :: String -> String
readExpr input = case Parsec.parse parseExpr "lisp" input of
  Left err -> "no match: " ++ show err
  Right val -> "found expression: " ++ show val
