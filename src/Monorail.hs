{-# LANGUAGE FlexibleContexts #-}

module Monorail (solve) where

import qualified Data.Map.Strict as Map
import Control.Monad.State (State, get, modify, runState)
import Control.Monad.Loops (untilM_)
import Text.Parsec.Prim (Stream, ParsecT, many, (<|>), parse)
import Text.Parsec.Combinator (eof, many1, endBy, option)
import Text.Parsec.Char (digit, endOfLine, string, oneOf, char)

newtype Register = Register Char deriving (Show, Ord, Eq)
data Machine = Machine { programCounter :: Int, registers :: Map.Map Register Int }
type Instruction = State Machine ()

machineStart = Machine { programCounter = 0, registers = Map.empty }

readRegister :: Register -> State Machine Int
readRegister r = do
  m <- get
  return $ Map.findWithDefault 0 r (registers m)

updateRegister :: (Int -> Int) -> Register -> State Machine ()
updateRegister f r = do
  currentVal <- readRegister r
  let newVal = f currentVal
  modify (\m -> m { registers = Map.insert r newVal (registers m) })

getProgramCounter :: State Machine Int
getProgramCounter = get >>= return . programCounter

advanceProgramCounter :: State Machine ()
advanceProgramCounter = jump 1

jump :: Int -> State Machine ()
jump relJump = modify (\m -> m { programCounter = programCounter m + relJump })

jumpNotZero :: Int -> Int -> State Machine ()
jumpNotZero relJump val = if (val == 0) then advanceProgramCounter else jump relJump

immediate :: Stream s m Char => ParsecT s u m Int
immediate = option id (char '-' >> return negate) <*> (read <$> many1 digit)

register :: Stream s m Char => ParsecT s u m Register
register = Register <$> oneOf "abcd"

copyImmediate :: Stream s m Char => ParsecT s u m Instruction
copyImmediate = (\x r -> updateRegister (const x) r >> advanceProgramCounter) <$> immediate <*> (char ' ' *> register)

copyRegister :: Stream s m Char => ParsecT s u m Instruction
copyRegister = (\src dst -> readRegister src >>= (\x -> updateRegister (const x) dst) >> advanceProgramCounter) <$> register <*> (char ' ' *> register)

copyInstruction :: Stream s m Char => ParsecT s u m Instruction
copyInstruction = string "cpy " *> (copyRegister <|> copyImmediate)

incInstruction :: Stream s m Char => ParsecT s u m Instruction
incInstruction = (\r -> updateRegister (+ 1) r >> advanceProgramCounter) <$> (string "inc " *> register)

decInstruction :: Stream s m Char => ParsecT s u m Instruction
decInstruction = (\r -> updateRegister (subtract 1) r >> advanceProgramCounter) <$> (string "dec " *> register)

unconditionalJump :: Stream s m Char => ParsecT s u m Instruction
unconditionalJump = jump <$> (immediate *> char ' ' *> immediate)

conditionalJump :: Stream s m Char => ParsecT s u m Instruction
conditionalJump = (\r pos -> readRegister r >>= jumpNotZero pos) <$> register <*> (char ' ' *> immediate)

jumpInstruction :: Stream s m Char => ParsecT s u m Instruction
jumpInstruction = string "jnz " *> (conditionalJump <|> unconditionalJump)

instruction :: Stream s m Char => ParsecT s u m Instruction
instruction = copyInstruction <|> incInstruction <|> decInstruction <|> jumpInstruction

instructionList :: Stream s m Char => ParsecT s u m [Instruction]
instructionList = instruction `endBy` endOfLine <* eof

runMachineUntilHalt :: [Instruction] -> State Machine (Map.Map Register Int)
runMachineUntilHalt xs = do
  let step = getProgramCounter >>= (\pc -> xs !! pc)
  let halted = getProgramCounter >>= (\pc -> return (pc >= length xs))

  step `untilM_` halted
  m <- get
  return (registers m)

solve :: String -> IO ()
solve input = do
  let parsed = parse instructionList "" input
  case parsed of
    Left err -> print err
    Right instructions -> do
      let (registerState, _) = runState (runMachineUntilHalt instructions) machineStart
      print registerState
