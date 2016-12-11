module SecurityThroughObscurity (solve) where

import Text.Parsec (parse)
import Text.Parsec.Char (digit, char, lower)
import Text.Parsec.Combinator (many1, eof)

data Room = Room { name :: String
                 , sectorId :: Int
                 , checksum :: String } deriving Show

eol = char '\n'

sectorIdParser = do
  res <- many1 digit
  return (read res :: Int)

roomName = do
  parts <- many1 (do
    res <- many1 lower
    char '-'
    return res)
  return . concat $ parts

roomLine = do
  rName <- roomName
  sId <- sectorIdParser
  char '['
  sum <- many1 lower
  char ']'
  eol
  return Room {name = rName,
               sectorId = sId,
               checksum = sum}

roomLines = do
  lines <- many1 roomLine
  eof
  return lines

solve :: String -> IO ()
solve input = do
  let roomData = parse roomLines "" input
  print roomData
