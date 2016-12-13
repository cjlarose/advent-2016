module SignalsAndNoise (solve) where

import Data.String (lines)
import Data.List (group, sort, transpose)
import Control.Arrow ((&&&))

mostFrequentChar :: String -> Char
mostFrequentChar = snd . minimum . map ((negate . length) &&& head) . group . sort

signal :: [String] -> String
signal = map mostFrequentChar . transpose

solve :: String -> IO ()
solve input = do
  let messages = lines input
  putStrLn $ signal messages
