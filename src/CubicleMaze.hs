{-# LANGUAGE FlexibleContexts #-}

module CubicleMaze (solve) where

import Text.Parsec.Prim (Stream, ParsecT, parse)
import Text.Parsec.Char (digit, endOfLine)
import Text.Parsec.Combinator (many1, eof)

designersNumber :: Stream s m Char => ParsecT s u m Int
designersNumber = read <$> (many1 digit <* endOfLine <* eof)

solve :: String -> IO ()
solve input = do
  let parsed = parse designersNumber "" input
  case parsed of
    Left err -> print err
    Right favoriteNumber -> print favoriteNumber
