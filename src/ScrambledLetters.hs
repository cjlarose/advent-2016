{-# LANGUAGE FlexibleContexts #-}

module ScrambledLetters (solve) where

import Data.List (elemIndex, permutations, find)
import Data.Maybe (fromJust)
import Text.Parsec.Prim (Stream, ParsecT, parse, (<|>), try)
import Text.Parsec.Char (endOfLine, string, char, digit, letter)
import Text.Parsec.Combinator (endBy, eof, many1)

type Instruction = String -> String
data Rotation = L | R

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

splitAt2 :: Int -> Int -> String -> (String, String, String)
splitAt2 i j str = (prefix, middle, suffix)
  where
    (prefix, rest) = splitAt i str
    (middle, suffix) = splitAt (j - i) rest

-------------------------------------------------------------------------------
-- Instructions
-------------------------------------------------------------------------------

rotateString :: Rotation -> Int -> String -> String
rotateString L n str = take (length str) . drop (n `mod` length str) $ str ++ str
rotateString R n str = take (length str) . drop (length str - (n `mod` length str)) $ str ++ str

rotateBasedOnPositionOfChar :: Char -> String -> String
rotateBasedOnPositionOfChar x str = rotateString R n str
  where
    i = fromJust $ elemIndex x str
    n = 1 + i + (if i >= 4 then 1 else 0)

swapPositions :: Int -> Int -> String -> String
swapPositions i j str | i > j = swapPositions j i str
                      | otherwise = prefix ++ [a] ++ middle ++ [b] ++ suffix
  where
    (prefix, b:middle, a:suffix) = splitAt2 i j str

swapChars :: Char -> Char -> String -> String
swapChars a b str = swapPositions (fromJust $ elemIndex a str) (fromJust $ elemIndex b str) str

reverseSubstring :: Int -> Int -> String -> String
reverseSubstring i j str = prefix ++ reverse middle ++ suffix
  where
    (prefix, middle, suffix) = splitAt2 i (j + 1) str

move :: Int -> Int -> String -> String
move i j str | i > j = let (prefix, middle, c:suffix) = splitAt2 j i str in prefix ++ [c] ++ middle ++ suffix
             | otherwise = let (prefix, c:middle, suffix) = splitAt2 i (j + 1) str in prefix ++ middle ++ [c] ++ suffix

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

unscramble :: [Instruction] -> String -> String
unscramble xs str = fromJust . find (\candidate -> scramble xs candidate == str) $ permutations str

solve :: String -> IO ()
solve input = do
  let parsed = parse instructionList "" input
  case parsed of
    Left err -> print err
    Right instructions -> do
      let password = "abcdefgh"
      let scrambed = scramble instructions password
      putStrLn scrambed

      let unscrambled = unscramble instructions "fbgdceah"
      putStrLn unscrambled
