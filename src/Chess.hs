module Chess (solve) where

import Numeric (showHex)
import Data.Char (isSpace)
import qualified Data.ByteString as B
import Crypto.Hash.MD5 (hash)
import Data.ByteString.UTF8 (fromString, ByteString(..))

allHashes :: String -> [ByteString]
allHashes prefix = map (hash . fromString . (\n -> prefix ++ show n)) [0..]

hasZerosPrefix :: Int -> String -> Bool
hasZerosPrefix n = all (== '0') . take n

toHex :: ByteString -> String
toHex = concatMap (`showHex` "") . B.unpack

doorCode :: String -> String
doorCode = take 8 .
           map (!! 5) .
           filter (hasZerosPrefix 5) .
           map toHex .
           allHashes

solve :: String -> IO ()
solve input = do
  let stripped = takeWhile (not . isSpace) input
  let code = doorCode stripped
  putStrLn code
