module Chess (solve) where

import Numeric (showHex)
import Data.Char (isSpace)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import qualified Crypto.Hash.MD5 as MD5

allHashes :: String -> [B.ByteString]
allHashes prefix = map (MD5.finalize . MD5.update prefixCtx . pack . show) [0..]
  where
    prefixCtx = MD5.update MD5.init $ pack prefix

hasZerosPrefix :: Int -> String -> Bool
hasZerosPrefix n = all (== '0') . take n

toHex :: B.ByteString -> String
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
