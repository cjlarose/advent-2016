{-# LANGUAGE FlexibleContexts #-}

module CubicleMaze (solve) where

import Data.Bits (popCount)
import Data.Maybe (fromJust)
import Data.List (foldl')
import qualified Data.OrdPSQ as PSQ
import qualified Data.Set as Set
import Text.Parsec.Prim (Stream, ParsecT, parse)
import Text.Parsec.Char (digit, endOfLine)
import Text.Parsec.Combinator (many1, eof)

type Maze = (Int, Int) -> Bool

designersNumber :: Stream s m Char => ParsecT s u m Int
designersNumber = read <$> (many1 digit <* endOfLine <* eof)

maze :: Int -> Maze
maze c (x, y) = even . popCount $ x*x + 3*x + 2*x*y + y + y*y + c

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = filter (\(a, b) -> a >= 0 && b >= 0) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

openSpaceNeighbors :: Maze -> (Int, Int) -> [(Int, Int)]
openSpaceNeighbors m xy = filter m $ neighbors xy

ucs :: Maze -> (Int, Int) -> PSQ.OrdPSQ (Int, Int) Int () -> Set.Set (Int, Int) -> Int
ucs m dst pq visited | minK == dst = minP
                     | otherwise = ucs m dst newQ (Set.insert minK visited)
  where
    (minK, minP, _) = fromJust . PSQ.findMin $ pq
    newQ = foldl' (\q n -> PSQ.insert n (minP + 1) () q) (PSQ.deleteMin pq) $ openSpaceNeighbors m minK

shortestPathLength :: Maze -> (Int, Int) -> (Int, Int) -> Int
shortestPathLength m src dst = ucs m dst (PSQ.singleton src 0 ()) Set.empty

possibleDestinations :: Maze -> (Int, Int) -> Int -> Set.Set (Int, Int)
possibleDestinations m src n = iterate stepOnce (Set.singleton src) !! n
  where
    stepOnce xs = Set.foldl' (\acc x -> Set.union acc (Set.fromList . openSpaceNeighbors m $ x)) xs xs

solve :: String -> IO ()
solve input = do
  let parsed = parse designersNumber "" input
  case parsed of
    Left err -> print err
    Right favoriteNumber -> do
      let cubicleMaze = maze favoriteNumber
      let pathLength = shortestPathLength cubicleMaze (1, 1) (31, 39)
      print pathLength

      let uniqDests = possibleDestinations cubicleMaze (1, 1) 50
      print . Set.size $ uniqDests
