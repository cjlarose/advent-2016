module TwoStepsForward (solve) where

import Data.List (intersect, foldl', maximumBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust, mapMaybe)
import Data.Bits ((.&.), shiftR)
import Data.Word (Word8)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Crypto.Hash.MD5 (hash)
import qualified Data.OrdPSQ as PSQ

data Direction = U | D | L | R deriving (Eq, Ord, Show)
type GridDimensions = (Int, Int)
type Position = (Int, Int)

inBounds :: GridDimensions -> Position -> Bool
inBounds (m, n) (i, j) = i >= 0 && i < m && j >= 0 && j < n

neighbors :: GridDimensions -> Position -> [Direction]
neighbors gd (i, j) = map fst . filter (inBounds gd . snd) . zip [U, D, L, R] $ adjacents
  where
    adjacents = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

nibbles :: Word8 -> (Word8, Word8)
nibbles b = ((b .&. 0xF0) `shiftR` 4, b .&. 0x0F)

openDoors :: String -> [Direction] -> [Direction]
openDoors prefix path = map fst . filter ((> 10) . snd) $ zip [U, D, L, R] [up, down, left, right]
  where
    h = hash . pack $ prefix ++ concatMap show path
    (up, down) = nibbles (h `B.index` 0)
    (left, right) = nibbles (h `B.index` 1)

adj :: Position -> Direction -> Position
adj (i, j) U = (i - 1, j)
adj (i, j) D = (i + 1, j)
adj (i, j) L = (i, j - 1)
adj (i, j) R = (i, j + 1)

stepOnce :: GridDimensions -> String -> (Position, [Direction]) -> [(Position, [Direction])]
stepOnce gd prefix (pos, ds) = map (\dir -> (adj pos dir, ds ++ [dir])) validDoors
  where
    validDoors = neighbors gd pos `intersect` openDoors prefix ds

search :: GridDimensions -> String -> Position -> PSQ.OrdPSQ [Direction] Int Position -> [Direction]
search gd prefix dst q | minV == dst = minK
                       | otherwise = search gd prefix dst newQ
  where
    (minK, minP, minV) = fromJust . PSQ.findMin $ q
    neighbors = stepOnce gd prefix (minV, minK)
    updateQ q (pos, dirs) = PSQ.insert dirs (minP + 1) pos q
    newQ = foldl' updateQ (PSQ.deleteMin q) neighbors

shortestPath :: GridDimensions -> String -> [Direction]
shortestPath gd@(m, n) prefix = search gd prefix (m - 1, n - 1) (PSQ.singleton [] 0 (0, 0))

longestPath' :: GridDimensions -> String -> (Position, [Direction]) -> Position -> Maybe [Direction]
longestPath' gd prefix src@(pos, path) dst | pos == dst = Just path
                                           | null adjacentPaths = Nothing
                                           | otherwise  = Just $ maximumBy (comparing length) adjacentPaths
  where
    neighbors = stepOnce gd prefix src
    adjacentPaths = mapMaybe (\neighbor -> longestPath' gd prefix neighbor dst) neighbors

longestPath :: GridDimensions -> String -> [Direction]
longestPath gd@(m, n) prefix = fromJust $ longestPath' gd prefix ((0, 0), []) (m - 1, n - 1)

solve :: String -> IO ()
solve input = do
  let passcode = head . lines $ input
  let gridDimensions = (4, 4)
  let directionList = shortestPath gridDimensions passcode
  putStrLn . concatMap show $ directionList

  let path = longestPath gridDimensions passcode
  print . length $ path
