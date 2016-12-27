module DragonChecksum (solve) where

import Data.List (find)
import Data.Maybe (fromJust)

expand :: [Bool] -> [Bool]
expand xs = xs ++ (False : (map not . reverse $ xs))

bitsToFillDisk :: [Bool] -> Int -> [Bool]
bitsToFillDisk xs n = take n . fromJust . find (\ys -> length ys >= n) . iterate expand $ xs

split :: [Bool] -> ([Bool], [Bool])
split [] = ([], [])
split (x:y:xs) = let (s1, s2) = split xs in (x:s1, y:s2)

checksumRound :: [Bool] -> [Bool]
checksumRound xs = let (odds, evens) = split xs in zipWith (==) odds evens

checksum :: [Bool] -> [Bool]
checksum = fromJust . find (odd . length) . iterate checksumRound

formatBits :: [Bool] -> [Char]
formatBits = map (\x -> if x then '1' else '0')

solve :: String -> IO ()
solve input = do
  let initialState = map (== '1') . head . lines $ input
  let finalStatePart1 = bitsToFillDisk initialState 272
  putStrLn . formatBits . checksum $ finalStatePart1

  let finalStatePart2 = bitsToFillDisk initialState 35651584
  putStrLn . formatBits . checksum $ finalStatePart2
