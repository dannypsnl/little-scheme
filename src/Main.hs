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

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (fmap show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "> ") evalAndPrint

readExpr :: String -> ThrowsError ScmValue
readExpr input = case parse parseExpr "scheme" input of
  Left err -> throwError $ Parser err
  Right val -> return val
