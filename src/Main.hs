module Main where
import qualified Interpreter                   as Interpreter
import           Parser

import qualified System.Environment            as Env
import qualified Text.ParserCombinators.Parsec as Parsec

main :: IO ()
main = Env.getArgs >>= print . Interpreter.eval . readExpr . head

readExpr :: String -> ScmValue
readExpr input = case Parsec.parse parseExpr "lisp" input of
  Left err  -> String $ "no match: " ++ show err
  Right val -> val
