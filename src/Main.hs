module Main where
import Interpreter (ScmError(..), ThrowsError, eval, extractValue, trapError)
import Parser (ScmValue(..), parseExpr)

import Control.Monad (liftM)
import Control.Monad.Except (throwError)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Parsec (parse)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    -- directly eval the input
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Program only takes 0 or 1 argument"

runRepl :: IO ()
runRepl = until_ (== "quit") evalAndPrint

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn
  where
    evalString :: String -> IO String
    evalString expr = return $ extractValue $ trapError (fmap show $ readExpr expr >>= eval)

until_ :: (String -> Bool) -> (String -> IO ()) -> IO ()
until_ pred action = do
  result <- readPrompt "> "
  if pred result
    then return ()
    else action result >> until_ pred action
  where
    readPrompt prompt = flushStr prompt >> getLine
    flushStr str = putStr str >> hFlush stdout

readExpr :: String -> ThrowsError ScmValue
readExpr input = case parse parseExpr "scheme" input of
  Left err -> throwError $ Parser err
  Right val -> return val
