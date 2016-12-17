{-# LANGUAGE FlexibleContexts #-}

module ExplosivesInCyberspace (solve) where

import Text.Parsec.Prim (Stream, ParsecT, parse, many, (<|>), getPosition, parserZero)
import Text.Parsec.Char (anyChar, digit, char, endOfLine, upper)
import Text.Parsec.Combinator (count, many1, eof, manyTill)
import Text.Parsec.Pos (sourceColumn)

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

literalBlock :: Stream s m Char => ParsecT s u m Block
literalBlock = Literal <$> many1 upper

columnAt pos = do
  currentPos <- sourceColumn <$> getPosition
  if currentPos == pos
    then (return ())
    else parserZero

version2Block :: Stream s m Char => ParsecT s u m Block
version2Block =
  literalBlock <|> do
    (length, n) <- marker
    currentCol <- sourceColumn <$> getPosition
    children <- manyTill version2Block (columnAt (currentCol + length))
    return (Compressed n children)

compressedData :: Stream s m Char => ParsecT s u m Block -> ParsecT s u m Block
compressedData blockParser = Compressed 1 <$> many blockParser <* endOfLine <* eof

decompressedLength :: Block -> Int
decompressedLength (Literal s) = length s
decompressedLength (Compressed n children) = n * (sum . map decompressedLength $ children)

solve :: String -> IO ()
solve input = do
  let parsedAsVersion1 = parse (compressedData version1Block) "" input
  case parsedAsVersion1 of
    Left err -> print err
    Right doc -> print $ decompressedLength doc

  let parsedAsVersion2 = parse (compressedData version2Block) "" input
  case parsedAsVersion2 of
    Left err -> print err
    Right doc -> print $ decompressedLength doc
