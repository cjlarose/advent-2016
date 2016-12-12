module Main where

import System.Environment (getArgs)
import qualified Taxicab
import qualified BathroomSecurity
import qualified SquaresWithThreeSides
import qualified SecurityThroughObscurity
import qualified Chess

solver :: Int -> (String -> IO ())
solver 1 = Taxicab.solve
solver 2 = BathroomSecurity.solve
solver 3 = SquaresWithThreeSides.solve
solver 4 = SecurityThroughObscurity.solve
solver 5 = Chess.solve

main :: IO ()
main = do
  args <- getArgs
  let problemNumber = head args
  contents <- readFile $ "inputs/" ++ problemNumber ++ ".txt"
  solver (read problemNumber) contents
