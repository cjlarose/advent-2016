module SignalsAndNoise (solve) where

import Data.String (lines)

solve :: String -> IO ()
solve input = do
  let messages = lines input
  print messages
