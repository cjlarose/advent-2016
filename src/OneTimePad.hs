module OneTimePad (solve) where

import Data.List (tails, isInfixOf)
import Data.Maybe (listToMaybe)
import Numeric (showHex)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.NibbleString as NS

windows :: Int -> [a] -> [[a]]
windows n xs = take (length xs - n + 1) . map (take n) . tails $ xs

toNibbleList :: B.ByteString -> [NS.Nibble]
toNibbleList bs = B.unpack bs >>= NS.byte2Nibbles

allHashes :: String -> [B.ByteString]
allHashes prefix = map hash [0..]
  where
    prefixCtx = MD5.update MD5.init $ pack prefix
    hash = MD5.finalize . MD5.update prefixCtx . pack . show

findTriplet :: [NS.Nibble] -> Maybe NS.Nibble
findTriplet ns = listToMaybe triplets
  where
    triplets = map head . filter (\[a, b, c] -> a == b && b == c) $ windows 3 ns

keys' :: [[NS.Nibble]] -> Int -> [(Int, [NS.Nibble])]
keys' (h:hs) i = if isKey then (i, h) : keys' hs (i + 1) else keys' hs (i + 1)
  where
    isKey = maybe False (\c -> any (isInfixOf (replicate 5 c)) (take 1000 hs)) $ findTriplet h

keys :: [B.ByteString] -> [(Int, [NS.Nibble])]
keys hashes = keys' (map toNibbleList hashes) 0

stretchHash :: B.ByteString -> B.ByteString
stretchHash = MD5.hash . pack . concatMap (`showHex` "") . toNibbleList

solve :: String -> IO ()
solve input = do
  let prefix = head . lines $ input
  let ks = keys $ allHashes prefix
  print . fst $ ks !! 63

  let stretchedHashes = map (\x -> iterate stretchHash x !! 2016) $ allHashes prefix
  let stretchedKeys = keys stretchedHashes
  print . fst $ stretchedKeys !! 63
