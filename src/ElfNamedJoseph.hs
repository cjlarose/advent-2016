module ElfNamedJoseph (solve) where

import Data.Bits (countLeadingZeros, finiteBitSize, clearBit, shiftL)

josephus :: Int -> Int
josephus n = clearBit n (finiteBitSize n - 1 - countLeadingZeros n) `shiftL` 1 + 1

solve :: String -> IO ()
solve input = do
  let n = read . head . lines $ input
  let winner = josephus n
  print winner
