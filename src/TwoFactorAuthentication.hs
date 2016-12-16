{-# LANGUAGE FlexibleContexts #-}

module TwoFactorAuthentication (solve) where

import Data.Array.Unboxed (UArray)
import Data.Array.IArray (array, (//), bounds, (!))
import Text.Parsec.Prim (Stream, ParsecT, many, (<|>), parse, try)
import Text.Parsec.Char (char, string, digit)
import Text.Parsec.Combinator (eof, many1)

data Instruction = DrawRect Int Int | RotateRow Int Int | RotateColumn Int Int deriving Show
data Screen = Screen (UArray (Int, Int) Bool)

instance Show Screen where
  show (Screen s) = unlines . map (\i -> map (charFor i) [jStart..jEnd]) $ [iStart..iEnd]
    where
      ((iStart, jStart), (iEnd, jEnd)) = bounds s
      charFor i j = if s ! (i, j) then '#' else '.'

eol :: Stream s m Char => ParsecT s u m Char
eol = char '\n'

intLiteral :: Stream s m Char => ParsecT s u m Int
intLiteral = read <$> many1 digit

rectInstruction :: Stream s m Char => ParsecT s u m Instruction
rectInstruction = DrawRect <$> (try (string "rect ") *> intLiteral) <*> (char 'x' *> intLiteral)

rotateRow :: Stream s m Char => ParsecT s u m Instruction
rotateRow = RotateRow <$> (string "row y=" *> intLiteral) <*> (string " by " *> intLiteral)

rotateColumn :: Stream s m Char => ParsecT s u m Instruction
rotateColumn = RotateColumn <$> (string "column x=" *> intLiteral) <*> (string " by " *> intLiteral)

rotateInstruction :: Stream s m Char => ParsecT s u m Instruction
rotateInstruction = string "rotate " *> (rotateRow <|> rotateColumn)

instruction :: Stream s m Char => ParsecT s u m Instruction
instruction = (rectInstruction <|> rotateInstruction) <* eol

instructionList :: Stream s m Char => ParsecT s u m [Instruction]
instructionList = many instruction <* eof

emptyScreen :: Screen
emptyScreen = Screen $ array ((0,0), (5,49)) []

updateScreen :: Screen -> Instruction -> Screen
updateScreen (Screen a) inst = Screen $ a // updates
  where
    maxJ = snd . snd . bounds $ a
    maxI = fst . snd . bounds $ a
    width = maxJ + 1
    height = maxI + 1
    updates = case inst of
                DrawRect w h -> [((i, j), True) | i <- [0..h - 1], j <- [0..w - 1]]
                RotateRow i n -> [((i, j), a ! (i, (j - n) `mod` width)) | j <- [0..maxJ]]
                RotateColumn j n -> [((i, j), a ! ((i - n) `mod` height, j)) | i <- [0..maxI]]

solve :: String -> IO ()
solve input = do
  let parsed = parse instructionList "" input
  case parsed of
    Left err -> print err
    Right instructions -> do
      print instructions
