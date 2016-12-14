module InternetProtocolVersion7 (solve) where

import Data.Char (isLower)
import Text.Parsec.Char (char, lower, satisfy)
import Text.Parsec.Prim (parse)

abba = do
  a <- lower
  b <- satisfy (\x -> isLower x && x /= a)
  char b
  char a

solve :: String -> IO ()
solve input = do
  let parsed = parse abba "" input
  print parsed
