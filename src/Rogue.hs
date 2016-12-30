module Rogue (solve) where

import Data.List (tails)

data Tile = Trap | Safe deriving (Show, Eq)

windows :: Int -> [a] -> [[a]]
windows n xs = take (length xs - n + 1) . map (take n) . tails $ xs

tileType :: Tile -> Tile -> Tile -> Tile
tileType Trap Trap Safe = Trap
tileType Safe Trap Trap = Trap
tileType Trap Safe Safe = Trap
tileType Safe Safe Trap = Trap
tileType _    _    _    = Safe

nextRow :: [Tile] -> [Tile]
nextRow xs = map (\[l, c, r] -> tileType l c r) . windows 3 $ [Safe] ++ xs ++ [Safe]

fromString :: String -> [Tile]
fromString = map (\x -> if x == '^' then Trap else Safe)

countSafe :: [Tile] -> Int
countSafe = length . filter (== Safe)

solve :: String -> IO ()
solve input = do
  let firstRow = fromString . head . lines $ input
  let first40Rows = take 40 . iterate nextRow $ firstRow
  let numSafeFirst40 = sum . map countSafe $ first40Rows
  print numSafeFirst40

  let allRows = take 400000 . iterate nextRow $ firstRow
  let numSafe = sum . map countSafe $ allRows
  print numSafe
