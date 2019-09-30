module Main where
import Core (Env, ScmValue(Atom, List, String), liftThrows)
import Interpreter (bindVars, eval, primitiveBindings, runIOThrows)
import Meta (defaultLibraryPath, littleSchemePath)
import Parser (readExpr)

import Control.Monad.Trans (liftIO)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Directory (copyFile, createDirectoryIfMissing, removeDirectoryRecursive)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runInputT defaultSettings (liftIO primitiveBindings >>= repl)
    ["cleanup"] -> cleanup
    ["init"] -> initLittleScheme
    -- directly eval the input
    [expr] -> runOne args
    _ -> putStrLn "Program only takes 0 or 1 argument"

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
runOne args = do
  env <- primitiveBindings >>=  (`bindVars` [("args", List $ map String $ drop 1 args)])
  runIOThrows (show <$> eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

initLittleScheme :: IO ()
initLittleScheme = do
  path <- defaultLibraryPath
  createDirectoryIfMissing True path
  copyFile "lib/stdlib.scm" (path </> "stdlib.scm")

cleanup :: IO ()
cleanup = do
  path <- littleSchemePath
  removeDirectoryRecursive path
