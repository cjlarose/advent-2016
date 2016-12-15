{-# LANGUAGE FlexibleContexts #-}

module InternetProtocolVersion7 (solve) where

import Data.Char (isLower)
import Text.Parsec.Prim (ParsecT, Stream, parse, many, try, (<|>))
import Text.Parsec.Char (char, lower, satisfy)
import Text.Parsec.Combinator (between, many1, eof)

data Sequence = NonHypernetSequence [String] | HypernetSequence [String] deriving Show
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

nonHypernetSequence :: Stream s m Char => ParsecT s u m Sequence
nonHypernetSequence = NonHypernetSequence <$> sequenceContents

hypernetSequence :: Stream s m Char => ParsecT s u m Sequence
hypernetSequence = HypernetSequence <$> between (char '[') (char ']') sequenceContents

none f = not . any f

supportsSnooping :: IPV7 -> Bool
supportsSnooping xs = any hasAbba nonHypernetSequences && none hasAbba hypernetSequences
  where
    hasAbba = any (\x -> length x > 1)
    nonHypernetSequences = [x | NonHypernetSequence x <- xs]
    hypernetSequences = [x | HypernetSequence x <- xs]

ipV7 :: Stream s m Char => ParsecT s u m IPV7
ipV7 = many (nonHypernetSequence <|> hypernetSequence) <* eol

ipList :: Stream s m Char => ParsecT s u m [IPV7]
ipList = many ipV7 <* eof

solve :: String -> IO ()
solve input = do
  let parsed = parse ipList "" input
  case parsed of
    Left err -> print err
    Right ips -> do
      let part1 = length . filter supportsSnooping $ ips
      print part1
