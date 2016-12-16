{-# LANGUAGE FlexibleContexts #-}

module ExplosivesInCyberspace (solve) where

import Text.Parsec.Prim (Stream, ParsecT, parse, many)
import Text.Parsec.Char (anyChar, digit, char, endOfLine)
import Text.Parsec.Combinator (count, many1, eof)

data Block = Literal String | Compressed Int [Block] deriving Show

intLiteral :: Stream s m Char => ParsecT s u m Int
intLiteral = read <$> many1 digit

marker :: Stream s m Char => ParsecT s u m (Int, Int)
marker = (\a b -> (a, b)) <$> (char '(' *> intLiteral <* char 'x') <*> (intLiteral <* char ')')

version1Block :: Stream s m Char => ParsecT s u m Block
version1Block = do
  (length, n) <- marker
  text <- count length anyChar
  return (Compressed n [Literal text])

compressedData :: Stream s m Char => ParsecT s u m Block -> ParsecT s u m Block
compressedData blockParser = Compressed 1 <$> many blockParser <* endOfLine <* eof

decompressedLength :: Block -> Int
decompressedLength (Literal s) = length s
decompressedLength (Compressed n children) = n * (sum . map decompressedLength $ children)

solve :: String -> IO ()
solve input = do
  let parsed = parse (compressedData version1Block) "" input
  case parsed of
    Left err -> print err
    Right doc -> do
      let len = decompressedLength doc
      print len
