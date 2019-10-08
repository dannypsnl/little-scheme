module Main where
import Core (Env, ScmValue(Atom, List, String), liftThrows)
import Interpreter (bindVars, eval, primitiveBindings, runIOThrows)
import Meta (defaultLibraryPath, littleSchemePath)
import Parser (readExpr)

import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (liftIO)
import Data.Either (isLeft)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  std <- littleSchemePath >>= doesDirectoryExist
  when (not std) initLittleScheme
  args <- getArgs
  case args of
    [] -> runInputT defaultSettings (liftIO primitiveBindings >>= repl)
    ["cleanup"] -> cleanup
    -- directly eval the input
    (file:args) -> runOne (file:args)

type Repl a = InputT IO a

repl :: Env -> Repl ()
repl env = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> outputStrLn "#bye"
    Just input -> liftIO (evalAndPrint env input) >> repl env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn
  where
    evalString :: Env -> String -> IO String
    evalString env expr = runIOThrows (fmap show $ (liftThrows (readExpr expr)) >>= eval env)

runOne :: [String] -> IO ()
runOne (file:args) = do
  env <- primitiveBindings >>=  (`bindVars` [("args", List $ map String args)])
  result <- runExceptT (eval env (List [Atom "load", String file]))
  case result of
    Left e -> hPutStrLn stderr (show e)
    Right _ -> putStr ""

initLittleScheme :: IO ()
initLittleScheme = do
  path <- defaultLibraryPath
  createDirectoryIfMissing True path
  copyFile "lib/stdlib.scm" (path </> "stdlib.scm")

cleanup :: IO ()
cleanup = do
  path <- littleSchemePath
  removeDirectoryRecursive path
