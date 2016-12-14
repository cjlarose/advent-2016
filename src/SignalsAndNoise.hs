module SignalsAndNoise (solve) where

import Data.String (lines)
import Data.List (group, sort, transpose)
import Control.Arrow ((&&&))

mostFrequentChar :: String -> Char
mostFrequentChar = snd . maximum . map (length &&& head) . group . sort

leastFrequentChar :: String -> Char
leastFrequentChar = snd . minimum . map (length &&& head) . group . sort

solve :: String -> IO ()
solve input = do
  let messages = transpose . lines $ input
  let mostFrequent = map mostFrequentChar messages
  let leastFrequent = map leastFrequentChar messages
  putStrLn mostFrequent
  putStrLn leastFrequent
