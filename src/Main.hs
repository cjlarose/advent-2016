module Main where

import System.IO (readFile)
import Text.Parsec (parse)
import Text.Parsec.Char (char, digit, string)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Combinator (many1, sepBy, endBy)

data Direction = L | R deriving Show

rotateR = do
  res <- char 'R'
  return R

rotateL = do
  res <- char 'L'
  return L

movement = do
  res <- many1 digit
  return (read res :: Int)

rotationMovement = do
  dir <- rotateR <|> rotateL
  steps <- movement
  return (dir, steps)

instructionList = (rotationMovement `sepBy` string ", ") `endBy` char '\n'

main :: IO ()
main = do
  contents <- readFile "inputs/01.txt"
  let result5 = parse instructionList "" contents
  print result5
