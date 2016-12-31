{-# LANGUAGE FlexibleContexts #-}

module ScrambledLetters (solve) where

import Text.Parsec.Prim (Stream, ParsecT, parse, (<|>), try)
import Text.Parsec.Char (endOfLine, string, char, digit, letter)
import Text.Parsec.Combinator (endBy, eof, many1)

type Instruction = String -> String
data Rotation = L | R

-------------------------------------------------------------------------------
-- Instructions
-------------------------------------------------------------------------------

rotateString :: Rotation -> Int -> String -> String
rotateString _ _ = id

rotateBasedOnPositionOfChar :: Char -> String -> String
rotateBasedOnPositionOfChar _ = id

swapPositions :: Int -> Int -> String -> String
swapPositions i j = id

swapChars :: Char -> Char -> String -> String
swapChars _ _ = id

reverseSubstring :: Int -> Int -> String -> String
reverseSubstring i j = id

move :: Int -> Int -> String -> String
move i j = id

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

rotation :: Stream s m Char => ParsecT s u m Rotation
rotation = (L <$ string "left") <|> (R <$ string "right")

nonNegativeInteger :: Stream s m Char => ParsecT s u m Int
nonNegativeInteger = read <$> many1 digit

rotateImmediate :: Stream s m Char => ParsecT s u m Instruction
rotateImmediate = rotateString <$> (rotation <* char ' ') <*> (1 <$ string "1 step" <|> (nonNegativeInteger <* string " steps"))

rotateRelative :: Stream s m Char => ParsecT s u m Instruction
rotateRelative = rotateBasedOnPositionOfChar <$> (string "based on position of letter " *> letter)

rotateInstruction :: Stream s m Char => ParsecT s u m Instruction
rotateInstruction = try (string "rotate ") *> (rotateImmediate <|> rotateRelative)

swapImmediate :: Stream s m Char => ParsecT s u m Instruction
swapImmediate = swapPositions <$> (string "position " *> nonNegativeInteger <* string " with position ") <*> nonNegativeInteger

swapRelative :: Stream s m Char => ParsecT s u m Instruction
swapRelative = swapChars <$> (string "letter " *> letter <* string " with letter ") <*> letter

swapInstruction :: Stream s m Char => ParsecT s u m Instruction
swapInstruction = string "swap " *> (swapImmediate <|> swapRelative)

reverseInstruction :: Stream s m Char => ParsecT s u m Instruction
reverseInstruction = reverseSubstring <$> (string "reverse positions " *> nonNegativeInteger <* string " through ") <*> nonNegativeInteger

moveInstruction :: Stream s m Char => ParsecT s u m Instruction
moveInstruction = move <$> (string "move position " *> nonNegativeInteger <* string " to position ") <*> nonNegativeInteger

instruction :: Stream s m Char => ParsecT s u m Instruction
instruction = rotateInstruction <|> swapInstruction <|> reverseInstruction <|> moveInstruction

instructionList :: Stream s m Char => ParsecT s u m [Instruction]
instructionList = instruction `endBy` endOfLine <* eof

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

scramble :: [Instruction] -> Instruction
scramble = foldl (flip (.)) id

solve :: String -> IO ()
solve input = do
  let parsed = parse instructionList "" input
  case parsed of
    Left err -> print err
    Right instructions -> do
      let password = "abcdefgh"
      let scrambed = scramble instructions password
      putStrLn scrambed
