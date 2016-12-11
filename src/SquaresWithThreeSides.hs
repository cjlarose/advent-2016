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
  return (a, b, c)

triplets = do
  res <- many1 tripletLine
  eof
  return res

isTriangle :: (Int, Int, Int) -> Bool
isTriangle (a, b, c) = a + b > c && a + c > b && b + c > a

transpose :: [(Int, Int, Int)] -> [(Int, Int, Int)]
transpose [] = []
transpose ((a, b, c) : (d, e, f) : (g, h, i) : xs) = (a, d, g) : (b, e, h) : (c, f, i) : transpose xs

solve :: String -> IO ()
solve input = do
  let triangleData = parse triplets "" input
  case triangleData of
    Left err -> print err
    Right ts -> do
      let countTriangles = length . filter isTriangle
      let numTriangles = countTriangles ts
      let numTrianglesTransposed = countTriangles . transpose $ ts
      print numTriangles
      print numTrianglesTransposed
