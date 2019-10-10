module Main where
import Scheme (prepareEnv, cleanup, runOne, createRepl)

import System.Environment (getArgs)

main :: IO ()
main = do
  prepareEnv
  args <- getArgs
  case args of
    [] -> createRepl
    ["cleanup"] -> cleanup
    -- directly eval the input
    (file:args) -> runOne (file:args)