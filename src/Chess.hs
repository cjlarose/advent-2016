module Chess (solve) where

import Numeric (showHex)
import Data.Char (isSpace)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.NibbleString as NS

allHashes :: String -> [B.ByteString]
allHashes prefix = map (MD5.finalize . MD5.update prefixCtx . pack . show) [0..]
  where
    prefixCtx = MD5.update MD5.init $ pack prefix

doorCode :: String -> String
doorCode = take 8 .
           map (head . (`showHex` "") . NS.head . NS.drop 5) .
           filter (NS.isPrefixOf expectedPrefix) .
           map NS.EvenNibbleString .
           allHashes
  where
    expectedPrefix = NS.pack $ replicate 5 0

solve :: String -> IO ()
solve input = do
  let stripped = takeWhile (not . isSpace) input
  let code = doorCode stripped
  putStrLn code
