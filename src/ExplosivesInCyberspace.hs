{-# LANGUAGE FlexibleContexts #-}

module ExplosivesInCyberspace (solve) where

import Text.Parsec.Prim (Stream, ParsecT, parse, many)
import Text.Parsec.Char (anyChar, digit, char, endOfLine)
import Text.Parsec.Combinator (count, many1, eof)

data Block = Literal String | Compressed Int Int Block
type Document = [Block]

intLiteral :: Stream s m Char => ParsecT s u m Int
intLiteral = read <$> many1 digit

marker :: Stream s m Char => ParsecT s u m (Int, Int)
marker = (\a b -> (a, b)) <$> (char '(' *> intLiteral <* char 'x') <*> (intLiteral <* char ')')

compressedSequence :: Stream s m Char => ParsecT s u m Block
compressedSequence = do
  (length, n) <- marker
  text <- count length anyChar
  return (Compressed length n (Literal text))

compressedData :: Stream s m Char => ParsecT s u m Document
compressedData = many compressedSequence <* endOfLine <* eof

decompressedLength :: Block -> Int
decompressedLength (Literal s) = length s
decompressedLength (Compressed _ n child) = n * decompressedLength child

solve :: String -> IO ()
solve input = do
  let parsed = parse compressedData "" input
  case parsed of
    Left err -> print err
    Right xs -> do
      let len = sum . map decompressedLength $ xs
      print len
