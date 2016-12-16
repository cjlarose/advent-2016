{-# LANGUAGE FlexibleContexts #-}

module ExplosivesInCyberspace (solve) where

import Text.Parsec.Prim (Stream, ParsecT, parse, many)
import Text.Parsec.Char (anyChar, digit, char)
import Text.Parsec.Combinator (count, many1, eof)

type CompressedSequence = ((Int, Int), String)

intLiteral :: Stream s m Char => ParsecT s u m Int
intLiteral = read <$> many1 digit

marker :: Stream s m Char => ParsecT s u m (Int, Int)
marker = (\a b -> (a, b)) <$> (char '(' *> intLiteral <* char 'x') <*> (intLiteral <* char ')')

compressedSequence :: Stream s m Char => ParsecT s u m CompressedSequence
compressedSequence = do
  (length, n) <- marker
  text <- count length anyChar
  return ((length, n), text)

compressedData :: Stream s m Char => ParsecT s u m [CompressedSequence]
compressedData = many compressedSequence <* char '\n' <* eof

decompressedLength :: [CompressedSequence] -> Int
decompressedLength = sum . map ((\(l, n) -> l * n) . fst)

solve :: String -> IO ()
solve input = do
  let parsed = parse compressedData "" input
  case parsed of
    Left err -> print err
    Right xs -> do
      let len = decompressedLength xs
      print len
