module Main where

import System.Environment (getArgs)
import qualified Taxicab

solver :: Int -> (String -> IO ())
solver 1 = Taxicab.solve

main :: IO ()
main = do
  args <- getArgs
  let problemNumber = head args
  contents <- readFile $ "inputs/" ++ problemNumber ++ ".txt"
  solver (read problemNumber) contents
