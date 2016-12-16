{-# LANGUAGE FlexibleContexts #-}

module TwoFactorAuthentication (solve) where

import Text.Parsec.Prim (Stream, ParsecT, many, (<|>), parse, try)
import Text.Parsec.Char (char, string, digit)
import Text.Parsec.Combinator (eof, many1)

data Instruction = DrawRect Int Int | RotateRow Int Int | RotateColumn Int Int deriving Show

eol :: Stream s m Char => ParsecT s u m Char
eol = char '\n'

intLiteral :: Stream s m Char => ParsecT s u m Int
intLiteral = read <$> many1 digit

rectInstruction :: Stream s m Char => ParsecT s u m Instruction
rectInstruction = DrawRect <$> (try (string "rect ") *> intLiteral) <*> (char 'x' *> intLiteral)

rotateRow :: Stream s m Char => ParsecT s u m Instruction
rotateRow = do
  string "row y="
  y <- intLiteral
  string " by "
  n <- intLiteral
  return (RotateRow y n)

rotateColumn :: Stream s m Char => ParsecT s u m Instruction
rotateColumn = do
  string "column x="
  x <- intLiteral
  string " by "
  n <- intLiteral
  return (RotateColumn x n)

rotateInstruction :: Stream s m Char => ParsecT s u m Instruction
rotateInstruction = do
  string "rotate "
  rotateRow <|> rotateColumn

instruction :: Stream s m Char => ParsecT s u m Instruction
instruction = (rectInstruction <|> rotateInstruction) <* eol

instructionList :: Stream s m Char => ParsecT s u m [Instruction]
instructionList = many instruction <* eof

solve :: String -> IO ()
solve input = do
  let parsed = parse instructionList "" input
  case parsed of
    Left err -> print err
    Right instructions -> do
      print instructions
