module Scheme (
  createRepl
  , runOne
  , prepareEnv
  , cleanup
) where
import Scheme.Core (Env, IOThrowsError, ScmValue(Atom, List, String), liftThrows)
import Scheme.Interpreter (bindVars, eval, primitiveBindings, runIOThrows)
import Scheme.Meta (defaultLibraryPath, littleSchemePath)
import Scheme.Parser (readExpr)

import Control.Monad (unless, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (liftIO)
import Data.Either (isLeft)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (hPrint, stderr)

prepareEnv :: IO ()
prepareEnv = do
  std <- littleSchemePath >>= doesDirectoryExist
  unless std initLittleScheme

createRepl :: IO ()
createRepl = runInputT defaultSettings (liftIO primitiveBindings >>= repl)

type Repl a = InputT IO a

repl :: Env -> Repl ()
repl env = do
  minput <- getInputLine "> "
  case minput of
    Nothing -> outputStrLn "#bye"
    Just input -> liftIO (evalAndPrint env input) >> repl env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env (liftThrows (readExpr expr)) >>= putStrLn
  where
    evalString :: Env -> IOThrowsError ScmValue -> IO String
    evalString env' expr' = runIOThrows (fmap show $ expr' >>= eval env')

runOne :: [String] -> IO ()
runOne (file:args) = do
  env <- primitiveBindings >>=  (`bindVars` [("args", List $ map String args)])
  result <- runExceptT (eval env (List [Atom "load", String file]))
  when (isLeft result) (hPrint stderr (show result))
runOne [] = putStrLn "No file provided!"

initLittleScheme :: IO ()
initLittleScheme = do
  path <- defaultLibraryPath
  createDirectoryIfMissing True path
  copyFile "lib/stdlib.scm" (path </> "stdlib.scm")

cleanup :: IO ()
cleanup = do
  path <- littleSchemePath
  removeDirectoryRecursive path
