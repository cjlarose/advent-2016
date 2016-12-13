module WarGames (solve) where

import Numeric (showHex)
import Data.Char (isSpace)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.NibbleString as NS

allHashes :: String -> [NS.NibbleString]
allHashes prefix = map (NS.EvenNibbleString . hash) [0..]
  where
    prefixCtx = MD5.update MD5.init $ pack prefix
    hash = MD5.finalize . MD5.update prefixCtx . pack . show

interestingPrefix :: NS.NibbleString
interestingPrefix = NS.pack $ replicate 5 0

interestingHash :: NS.NibbleString -> Bool
interestingHash = NS.isPrefixOf interestingPrefix

doorCode :: String -> String
doorCode = take 8 .
           map (head . (`showHex` "") . NS.head . NS.drop 5) .
           filter interestingHash .
           allHashes

solve :: String -> IO ()
solve input = do
  let stripped = takeWhile (not . isSpace) input
  let code = doorCode stripped
  putStrLn code
