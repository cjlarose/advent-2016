module TwoStepsForward (solve) where

import Data.List (intersect, foldl')
import Data.Maybe (fromJust)
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

search :: GridDimensions -> String -> Position -> PSQ.OrdPSQ [Direction] Int Position -> [Direction]
search gd prefix dst q | minV == dst = minK
                       | otherwise = search gd prefix dst newQ
  where
    (minK, minP, minV) = fromJust . PSQ.findMin $ q
    validDoors = neighbors gd minV `intersect` openDoors prefix minK
    updateQ q dir = PSQ.insert (minK ++ [dir]) (minP + 1) (adj minV dir) q
    newQ = foldl' updateQ (PSQ.deleteMin q) validDoors

shortestPath :: GridDimensions -> String -> [Direction]
shortestPath gd@(m, n) prefix = search gd prefix (m - 1, n - 1) (PSQ.singleton [] 0 (0, 0))

solve :: String -> IO ()
solve input = do
  let passcode = head . lines $ input
  let directionList = shortestPath (4, 4) passcode
  putStrLn . concatMap show $ directionList
