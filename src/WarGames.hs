module WarGames (solve) where

import Numeric (showHex)
import Data.Char (isSpace)
import Data.List (any, sort)
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

index :: NS.NibbleString -> Int -> NS.Nibble
index ns i = NS.head . NS.drop i $ ns

nibbleToHexChar :: NS.Nibble -> Char
nibbleToHexChar = head . (`showHex` "")

doorCode :: String -> String
doorCode = take 8 .
           map (nibbleToHexChar . (`index` 5)) .
           filter interestingHash .
           allHashes

nonSequentialDoorCode :: String -> String
nonSequentialDoorCode = assembleCode [] .
                        map positionAndValue .
                        filter validPosition .
                        filter interestingHash .
                        allHashes
  where
    validPosition ns = index ns 5 < 8
    positionAndValue ns = (fromIntegral $ index ns 5, nibbleToHexChar $ index ns 6)
    assembleCode :: [(Int, Char)] -> [(Int, Char)] -> String
    assembleCode keep (x@(i,_):xs) | length keep == 8 = map snd . sort $ keep
                                   | otherwise = if any ((== i) . fst) keep
                                                 then assembleCode keep xs
                                                 else assembleCode (x:keep) xs

solve :: String -> IO ()
solve input = do
  let doorId = takeWhile (not . isSpace) input
  let code = doorCode doorId
  let nonSequentialCode = nonSequentialDoorCode doorId
  putStrLn code
  putStrLn nonSequentialCode
