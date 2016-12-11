module SquaresWithThreeSides (solve) where

import Text.Parsec (parse)
import Text.Parsec.Char (digit, char)
import Text.Parsec.Prim ((<|>), skipMany)
import Text.Parsec.Combinator (many1, eof, skipMany1, sepBy, sepBy1)

eol = char '\n'

sideLength = do
  res <- many1 digit
  return (read res :: Int)

tripletLine = do
  skipMany (char ' ')
  a <- sideLength
  skipMany1 (char ' ')
  b <- sideLength
  skipMany1 (char ' ')
  c <- sideLength
  eol
  return [a, b, c]

triplets = do
  res <- many1 tripletLine
  eof
  return res

solve :: String -> IO ()
solve input = do
  let triangleData = parse triplets "" input
  print triangleData
