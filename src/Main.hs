module Main where

import System.Environment (getArgs)
import qualified Taxicab
import qualified BathroomSecurity

solver :: Int -> (String -> IO ())
solver 1 = Taxicab.solve
solver 2 = BathroomSecurity.solve

main :: IO ()
main = do
  args <- getArgs
  let problemNumber = head args
  contents <- readFile $ "inputs/" ++ problemNumber ++ ".txt"
  solver (read problemNumber) contents
