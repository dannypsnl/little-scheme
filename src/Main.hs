module Main where
import           Interpreter        (eval)
import           Parser             (ScmValue (..), parseExpr)

import           System.Environment (getArgs)
import           Text.Parsec        (parse)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

readExpr :: String -> ScmValue
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> String $ "no match: " ++ show err
  Right val -> val
