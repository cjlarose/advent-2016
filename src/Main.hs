module Main where

import System.IO (readFile)
import Text.Parsec (parse)
import Text.Parsec.Char (char, digit, string)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Combinator (many1, sepBy, endBy)

data Direction = L | R deriving Show

rotateR = do
  res <- char 'R'
  return R

rotateL = do
  res <- char 'L'
  return L

movement = do
  res <- many1 digit
  return (read res :: Int)

rotationMovement = do
  dir <- rotateR <|> rotateL
  steps <- movement
  return (dir, steps)

instructionList = (rotationMovement `sepBy` string ", ") `endBy` char '\n'

newtype Orientation = Orientation (Int, Int) deriving Show
newtype Displacement = Displacement (Int, Int) deriving Show

rotate :: Orientation -> Direction -> Orientation
rotate (Orientation (x, y)) L = Orientation (-y, x)
rotate (Orientation (x, y)) R = Orientation (y, -x)

delta :: Orientation -> Int -> Displacement
delta (Orientation (dx, dy)) d = Displacement (d * dx, d * dy)

addDisplacement :: Displacement -> Displacement -> Displacement
addDisplacement (Displacement (x1, y1)) (Displacement (x2, y2)) = Displacement (x1 + x2, y1 + y2)

move :: (Orientation, Displacement) -> (Direction, Int) -> (Orientation, Displacement)
move (v, d) (dir, steps) = (newOrientation, newDisplacement)
  where
    newOrientation = rotate v dir
    newDisplacement = addDisplacement d (delta newOrientation steps)

taxicabDistance :: Displacement -> Int
taxicabDistance (Displacement (dx, dy)) = abs dx + abs dy

main :: IO ()
main = do
  contents <- readFile "inputs/01.txt"
  let initialDisplacement = Displacement (0, 0)
  let initialOrientation = Orientation (0, 1)
  let instructions = parse instructionList "" contents
  case instructions of
    Left err -> print err
    Right (is:_) -> do
      let (_, finalDisplacement) = foldl move (initialOrientation, initialDisplacement) is
      print . taxicabDistance $ finalDisplacement
