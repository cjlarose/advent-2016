module Taxicab (solve) where

import System.IO (readFile)
import Text.Parsec (parse)
import Text.Parsec.Char (char, digit, string)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Combinator (many1, sepBy, endBy, eof)

data Direction = L | R deriving Show
data Orientation = N | E | S | W deriving Show
newtype Displacement = Displacement (Int, Int) deriving (Show, Eq)

rotateR = R <$ char 'R'
rotateL = L <$ char 'L'

movement = read <$> many1 digit

rotationMovement = (\a b -> (a, b)) <$> (rotateR <|> rotateL) <*> movement

instructionList = ((rotationMovement `sepBy` string ", ") <* char '\n') <* eof

rotate :: Orientation -> Direction -> Orientation
rotate N R = E
rotate E R = S
rotate S R = W
rotate W R = N
rotate N L = W
rotate W L = S
rotate S L = E
rotate E L = N

cardinalize :: Orientation -> [(Direction, Int)] -> [(Orientation, Int)]
cardinalize v [] = []
cardinalize v ((dir, steps):xs) = let newDir = rotate v dir in (newDir, steps) : cardinalize newDir xs

step1 :: Orientation -> Displacement -> Displacement
step1 N (Displacement (x, y)) = Displacement (x, y + 1)
step1 E (Displacement (x, y)) = Displacement (x + 1, y)
step1 S (Displacement (x, y)) = Displacement (x, y - 1)
step1 W (Displacement (x, y)) = Displacement (x - 1, y)

advance :: Displacement -> Orientation -> Int -> [Displacement]
advance d c n = tail . take (n + 1) $ iterate (step1 c) d

displacements :: Displacement -> [(Orientation, Int)] -> [Displacement]
displacements di = reverse . foldl f [di]
  where
    f :: [Displacement] -> (Orientation, Int) -> [Displacement]
    f (d:ds) (c, n) = let newDs = reverse (advance d c n) in newDs ++ (d:ds)

taxicabDistance :: Displacement -> Int
taxicabDistance (Displacement (dx, dy)) = abs dx + abs dy

takeUpToDuplicate :: Eq x => [x] -> [x]
takeUpToDuplicate xs = f xs []
  where
    f [] _ = []
    f (x:xs) ys = if x `elem` ys then [x] else x : f xs (x:ys)

solve :: String -> IO ()
solve input = do
  let initialDisplacement = Displacement (0, 0)
  let initialOrientation = N
  let instructions = parse instructionList "" input
  case instructions of
    Left err -> print err
    Right is -> do
      let cs = cardinalize initialOrientation is
      let ds = displacements initialDisplacement cs
      let step1Answer = taxicabDistance . last $ ds
      let step2Answer = taxicabDistance . last . takeUpToDuplicate $ ds
      putStrLn $ "Step 1: " ++ show step1Answer
      putStrLn $ "Step 2: " ++ show step2Answer
