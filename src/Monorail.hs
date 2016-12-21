{-# LANGUAGE FlexibleContexts #-}

module Monorail (solve) where

import Text.Parsec.Prim (Stream, ParsecT, many, (<|>), parse)
import Text.Parsec.Combinator (eof, many1, endBy, option)
import Text.Parsec.Char (digit, endOfLine, string, oneOf, char)
import Debug.Trace (traceShow)

newtype Register = Register Char deriving Show
data Instruction = CopyImmediate Int Register | CopyRegister Register Register | Inc Register | Dec Register | Jump Int | JumpNonZero Register Int deriving Show

immediate :: Stream s m Char => ParsecT s u m Int
immediate = option id (char '-' >> return negate) <*> (read <$> many1 digit)

register :: Stream s m Char => ParsecT s u m Register
register = Register <$> oneOf "abcd"

copyImmediate :: Stream s m Char => ParsecT s u m Instruction
copyImmediate = CopyImmediate <$> immediate <*> (char ' ' *> register)

copyRegister :: Stream s m Char => ParsecT s u m Instruction
copyRegister = CopyRegister <$> register <*> (char ' ' *> register)

copyInstruction :: Stream s m Char => ParsecT s u m Instruction
copyInstruction = string "cpy " *> (copyRegister <|> copyImmediate)

incInstruction :: Stream s m Char => ParsecT s u m Instruction
incInstruction = Inc <$> (string "inc " *> register)

decInstruction :: Stream s m Char => ParsecT s u m Instruction
decInstruction = Dec <$> (string "dec " *> register)

unconditionalJump :: Stream s m Char => ParsecT s u m Instruction
unconditionalJump = Jump <$> (immediate *> char ' ' *> immediate)

conditionalJump :: Stream s m Char => ParsecT s u m Instruction
conditionalJump = JumpNonZero <$> register <*> (char ' ' *> immediate)

jumpInstruction :: Stream s m Char => ParsecT s u m Instruction
jumpInstruction = string "jnz " *> (conditionalJump <|> unconditionalJump)

instruction :: Stream s m Char => ParsecT s u m Instruction
instruction = copyInstruction <|> incInstruction <|> decInstruction <|> jumpInstruction

instructionList :: Stream s m Char => ParsecT s u m [Instruction]
instructionList = instruction `endBy` endOfLine <* eof

solve :: String -> IO ()
solve input = do
  let parsed = parse instructionList "" input
  case parsed of
    Left err -> print err
    Right instructions -> do
      print instructions
