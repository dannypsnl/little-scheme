module Main where
import Interpreter (ScmError(..), ThrowsError, eval, extractValue, trapError)
import Parser (ScmValue(..), parseExpr)

import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Environment (getArgs)
import Text.Parsec (parse)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runInputT defaultSettings repl
    -- directly eval the input
    [expr] -> evalAndPrint expr
    _ -> putStrLn "Program only takes 0 or 1 argument"

type Repl a = InputT IO a

repl :: Repl ()
repl = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> outputStrLn "#bye"
    Just input -> (liftIO $ evalAndPrint input) >> repl

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn
  where
    evalString :: String -> IO String
    evalString expr = return $ extractValue $ trapError (fmap show $ readExpr expr >>= eval)

readExpr :: String -> ThrowsError ScmValue
readExpr input = case parse parseExpr "scheme" input of
  Left err -> throwError $ Parser err
  Right val -> return val
