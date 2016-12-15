{-# LANGUAGE FlexibleContexts #-}

module InternetProtocolVersion7 (solve) where

import Data.Char (isLower)
import Text.Parsec.Prim (ParsecT, Stream, parse, many, try, (<|>))
import Text.Parsec.Char (char, lower, satisfy)
import Text.Parsec.Combinator (between, many1, eof)

data Sequence = SupernetSequence [String] | HypernetSequence [String] deriving Show
type IPV7 = [Sequence]

eol :: Stream s m Char => ParsecT s u m Char
eol = char '\n'

abba :: Stream s m Char => ParsecT s u m String
abba = do
  a <- lower
  b <- satisfy (\x -> isLower x && x /= a)
  char b
  char a
  return [a, b, b, a]

sequenceContents :: Stream s m Char => ParsecT s u m [String]
sequenceContents = many1 (try abba <|> (pure <$> lower))

supernetSequence :: Stream s m Char => ParsecT s u m Sequence
supernetSequence = SupernetSequence <$> sequenceContents

hypernetSequence :: Stream s m Char => ParsecT s u m Sequence
hypernetSequence = HypernetSequence <$> between (char '[') (char ']') sequenceContents

none :: (a -> Bool) -> [a] -> Bool
none f = not . any f

ipV7 :: Stream s m Char => ParsecT s u m IPV7
ipV7 = many (supernetSequence <|> hypernetSequence) <* eol

ipList :: Stream s m Char => ParsecT s u m [IPV7]
ipList = many ipV7 <* eof

supportsSnooping :: IPV7 -> Bool
supportsSnooping xs = any hasAbba supernetSequences && none hasAbba hypernetSequences
  where
    hasAbba = any (\x -> length x > 1)
    supernetSequences = [x | SupernetSequence x <- xs]
    hypernetSequences = [x | HypernetSequence x <- xs]

solve :: String -> IO ()
solve input = do
  let parsed = parse ipList "" input
  case parsed of
    Left err -> print err
    Right ips -> do
      let part1 = length . filter supportsSnooping $ ips
      print part1
