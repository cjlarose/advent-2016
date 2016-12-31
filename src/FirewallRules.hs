{-# LANGUAGE FlexibleContexts #-}

module FirewallRules (solve) where

import Data.Word (Word32)
import Text.Parsec.Prim (parse, Stream, ParsecT)
import Text.Parsec.Char (char, digit, endOfLine)
import Text.Parsec.Combinator (many1, endBy, eof)

ip :: Stream s m Char => ParsecT s u m Word32
ip = read <$> many1 digit

ipRange :: Stream s m Char => ParsecT s u m (Word32, Word32)
ipRange = (\x y -> (x, y)) <$> (ip <* char '-') <*> ip

ipRangeList :: Stream s m Char => ParsecT s u m [(Word32, Word32)]
ipRangeList = ipRange `endBy` endOfLine <* eof

solve :: String -> IO ()
solve input = do
  let parsed = parse ipRangeList "" input
  case parsed of
    Left err -> print err
    Right ipRanges -> do
      print ipRanges
