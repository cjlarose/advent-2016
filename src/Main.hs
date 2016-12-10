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
newtype Position = Position (Int, Int) deriving Show
newtype Displacement = Displacement (Int, Int) deriving Show

rotate :: Orientation -> Direction -> Orientation
rotate (Orientation (x, y)) L = Orientation (-y, x)
rotate (Orientation (x, y)) R = Orientation (y, -x)

delta :: Orientation -> Int -> Displacement
delta (Orientation (dx, dy)) d = Displacement (d * dx, d * dy)

advancePosition :: Position -> Displacement -> Position
advancePosition (Position (x, y)) (Displacement (dx, dy)) = Position (x + dx, y + dy)

move :: (Orientation, Position) -> (Direction, Int) -> (Orientation, Position)
move (v, p) (dir, steps) = (newOrientation, newPosition)
  where
    newOrientation = rotate v dir
    newPosition = advancePosition p (delta newOrientation steps)

displacement :: Position -> Position -> Displacement
displacement (Position (x1, y1)) (Position (x2, y2)) = Displacement (x2 - x1, y2 - y1)

taxicabDistance :: Displacement -> Int
taxicabDistance (Displacement (dx, dy)) = abs dx + abs dy

main :: IO ()
main = do
  contents <- readFile "inputs/01.txt"
  let initialPosition = Position (0, 0)
  let initialOrientation = Orientation (0, 1)
  let instructions = parse instructionList "" contents
  case instructions of
    Left err -> print err
    Right (is:_) -> do
      let (_, finalPosition) = foldl move (initialOrientation, initialPosition) is
      let totalDisplacement = displacement initialPosition finalPosition
      print . taxicabDistance $ totalDisplacement
