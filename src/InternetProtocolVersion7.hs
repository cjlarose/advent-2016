{-# LANGUAGE FlexibleContexts #-}

module InternetProtocolVersion7 (solve) where

import Data.Char (isLower)
import Data.List (tails, null)
import qualified Data.Set as Set
import Text.Parsec.Prim (ParsecT, Stream, parse, many, try, (<|>))
import Text.Parsec.Char (char, lower, satisfy)
import Text.Parsec.Combinator (between, many1, eof)

data Sequence = SupernetSequence String | HypernetSequence String deriving Show
type IPV7 = [Sequence]

eol :: Stream s m Char => ParsecT s u m Char
eol = char '\n'

supernetSequence :: Stream s m Char => ParsecT s u m Sequence
supernetSequence = SupernetSequence <$> many1 lower

hypernetSequence :: Stream s m Char => ParsecT s u m Sequence
hypernetSequence = HypernetSequence <$> between (char '[') (char ']') (many1 lower)

ipV7 :: Stream s m Char => ParsecT s u m IPV7
ipV7 = many (supernetSequence <|> hypernetSequence) <* eol

ipList :: Stream s m Char => ParsecT s u m [IPV7]
ipList = many ipV7 <* eof

abba :: Stream s m Char => ParsecT s u m String
abba = do
  a <- lower
  b <- satisfy (\x -> isLower x && x /= a)
  char b
  char a
  return [a, b, b, a]

hasAbba :: String -> Bool
hasAbba s = do
  let parsed = parse (many1 ((const True <$> try abba) <|> (const False <$> lower))) "" s
  case parsed of
    Left _ -> False
    Right xs -> or xs

none :: (a -> Bool) -> [a] -> Bool
none f = not . any f

supportsTLS :: IPV7 -> Bool
supportsTLS xs = any hasAbba supernetSequences && none hasAbba hypernetSequences
  where
    supernetSequences = [x | SupernetSequence x <- xs]
    hypernetSequences = [x | HypernetSequence x <- xs]

windows :: Int -> [a] -> [[a]]
windows n xs = take (length xs - n + 1) . map (take n) . tails $ xs

abas :: String -> [String]
abas = filter (\[a, b, c] -> a == c && a /= b) . windows 3

supportsSSL :: IPV7 -> Bool
supportsSSL xs = not . null $ Set.intersection (Set.map toBab allAbas) allBabs
  where
    allAbas = Set.fromList . concatMap abas $ [x | SupernetSequence x <- xs]
    allBabs = Set.fromList . concatMap abas $ [x | HypernetSequence x <- xs]
    toBab [a, b, c] = [b, a, b]

solve :: String -> IO ()
solve input = do
  let parsed = parse ipList "" input
  case parsed of
    Left err -> print err
    Right ips -> do
      let numTLS = length . filter supportsTLS $ ips
      let numSSL = length . filter supportsSSL $ ips
      print numTLS
      print numSSL
