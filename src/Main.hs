module Main where
import Interpreter (ScmError(..), ThrowsError, eval, extractValue, trapError)
import Parser (ScmValue(..), parseExpr)

import Control.Monad (liftM)
import Control.Monad.Except (throwError)
import System.Environment (getArgs)
import Text.Parsec (parse)

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled

readExpr :: String -> ThrowsError ScmValue
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val
