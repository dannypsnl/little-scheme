module Main where
import Interpreter (Env, ScmError(..), ThrowsError, eval, extractValue, liftThrows, nullEnv, runIOThrows, trapError)
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
    [] -> runInputT defaultSettings (liftIO nullEnv >>= repl)
    -- directly eval the input
    [expr] -> nullEnv >>= flip evalAndPrint expr
    _ -> putStrLn "Program only takes 0 or 1 argument"

type Repl a = InputT IO a

repl :: Env -> Repl ()
repl env = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> outputStrLn "#bye"
    Just input -> liftIO (evalAndPrint env input) >> (repl env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn
  where
    evalString :: Env -> String -> IO String
    evalString env expr = runIOThrows (fmap show $ (liftThrows $ readExpr expr) >>= eval env)

readExpr :: String -> ThrowsError ScmValue
readExpr input = case parse parseExpr "scheme" input of
  Left err -> throwError $ Parser err
  Right val -> return val
