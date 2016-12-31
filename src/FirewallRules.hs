{-# LANGUAGE FlexibleContexts #-}

module FirewallRules (solve) where

import Data.Word (Word32)
import Data.List (foldl')
import Text.Parsec.Prim (parse, Stream, ParsecT)
import Text.Parsec.Char (char, digit, endOfLine)
import Text.Parsec.Combinator (many1, endBy, eof)

ip :: Stream s m Char => ParsecT s u m Word32
ip = read <$> many1 digit

ipRange :: Stream s m Char => ParsecT s u m (Word32, Word32)
ipRange = (\x y -> (x, y)) <$> (ip <* char '-') <*> ip

ipRangeList :: Stream s m Char => ParsecT s u m [(Word32, Word32)]
ipRangeList = ipRange `endBy` endOfLine <* eof

excludeFromRange :: (Word32, Word32) -> (Word32, Word32) -> [(Word32, Word32)]
excludeFromRange (min, max) (lo, hi) | lo < min && hi < min || lo > max && hi > max = [(min, max)]
                                     | lo <= min && hi == min = [(min + 1, max)]
                                     | lo <= min && hi > min && hi < max = [(hi + 1, max)]
                                     | lo <= min && hi >= max = []
                                     | lo > min && lo < max && hi > min && hi < max = [(min, lo -1), (hi + 1, max)]
                                     | lo > min && lo < max && hi >= max = [(min, lo - 1)]
                                     | lo == max && hi >= max = [(min, hi - 1)]

permittedRanges :: [(Word32, Word32)] -> [(Word32, Word32)]
permittedRanges = foldl' f [(minBound, maxBound)]
  where
    f includes exclude = concatMap (`excludeFromRange` exclude) includes

solve :: String -> IO ()
solve input = do
  let parsed = parse ipRangeList "" input
  case parsed of
    Left err -> print err
    Right ipRanges -> do
      let ranges = permittedRanges ipRanges
      print . fst . head $ ranges
