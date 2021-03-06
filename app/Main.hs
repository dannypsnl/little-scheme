module Main where
import Scheme (cleanup, createRepl, runOne)

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> createRepl
    ["cleanup"] -> cleanup
    -- directly eval the input
    (file:args') -> runOne (file:args')
