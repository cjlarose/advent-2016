module SecurityThroughObscurity (solve) where

import Data.Char (ord, chr)
import Data.List (group, sort, sortOn, find)
import Text.Parsec (parse)
import Text.Parsec.Char (digit, char, lower)
import Text.Parsec.Combinator (many1, eof)

data Room = Room { name :: [String]
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
  return parts

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

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = map (\x -> (head x, length x)) . group . sort

calcChecksum :: Ord a => [a] -> [a]
calcChecksum = map fst . take 5 . sortOn (\(x, f) -> (-f, x)) . frequencies

realRoom :: Room -> Bool
realRoom room = checksum room == calcChecksum (concat . name  $ room)

shiftChar :: Int -> Char -> Char
shiftChar n c = chr $ ord 'a' + (ord c - ord 'a' + n) `mod` 26

decryptedName :: Room -> String
decryptedName room = unwords . map decryptPart . name $ room
  where
    decryptPart = map (shiftChar (sectorId room))

solve :: String -> IO ()
solve input = do
  let parseResult = parse roomLines "" input
  case parseResult of
    Left err -> print err
    Right roomData -> do
      let realRooms = filter realRoom roomData
      let sectorIdSum = sum . map sectorId $ realRooms
      let northPoleRoom = find (\r -> decryptedName r == "northpole object storage") realRooms
      print sectorIdSum
      print northPoleRoom
