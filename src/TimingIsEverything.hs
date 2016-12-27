{-# LANGUAGE FlexibleContexts #-}

module TimingIsEverything (solve) where

import Data.List (find)
import Data.Maybe (fromJust)
import Text.Parsec.Prim (Stream, ParsecT, parse)
import Text.Parsec.Char (endOfLine, string, digit)
import Text.Parsec.Combinator (endBy, many1)

type Disc = Int -> Bool

integerLiteral :: Stream s m Char => ParsecT s u m Int
integerLiteral = read <$> many1 digit

disk :: Stream s m Char => ParsecT s u m Disc
disk = (\n i t -> ((t + i) `mod` n) == 0) <$>
       (string "Disc #" *> many1 digit *> string " has " *> integerLiteral) <*>
       (string " positions; at time=0, it is at position " *> integerLiteral <* string ".")

diskList :: Stream s m Char => ParsecT s u m [Disc]
diskList = disk `endBy` endOfLine

fallsThroughAllDiscs :: [Disc] -> Int -> Bool
fallsThroughAllDiscs xs t = and $ zipWith (\f x -> f x) xs [t + 1..]

timeToDrop :: [Disc] -> Int
timeToDrop xs = fromJust $ find (fallsThroughAllDiscs xs) [0..]

solve :: String -> IO ()
solve input = do
  let parsed = parse diskList "" input
  case parsed of
    Left err -> print err
    Right ds -> do
      let t = timeToDrop ds
      print t

      let newDisk t = (t `mod` 11) == 0
      let newDs = ds ++ [newDisk]
      let newT = timeToDrop newDs
      print newT
