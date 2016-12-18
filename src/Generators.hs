{-# LANGUAGE FlexibleContexts #-}

module Generators (solve) where

import qualified Data.Set as Set
import Text.Parsec.Prim (Stream, ParsecT, parse, (<|>), try)
import Text.Parsec.Char (char, string, endOfLine, letter)
import Text.Parsec.Combinator (many1, sepBy, option, optional, eof, endBy)

type Element = String
data ComponentType = Generator | Microchip deriving (Show, Ord, Eq)
type Component = (Element, ComponentType)
type Floor = Set.Set Component
type FacilityZipper = ([Floor], Floor, [Floor]) -- floors above, current floor, floors below

component :: Stream s m Char => ParsecT s u m Component
component = (\e t -> (e, t)) <$> many1 letter <*> (generator <|> microchip)
  where
    generator = Generator <$ string " generator"
    microchip = Microchip <$ string "-compatible microchip"

componentList :: Stream s m Char => ParsecT s u m Floor
componentList = (Set.empty <$ string "nothing relevant") <|>
                Set.union <$>
                (string "a " *> (Set.fromList <$> component `sepBy` try (string ", a "))) <*>
                (option Set.empty (Set.singleton <$> lastItem))
  where
    lastItem = optional (char ',') *> string " and a " *> component

facilityDescription :: Stream s m Char => ParsecT s u m [Floor]
facilityDescription = floorDescription `endBy` endOfLine <* eof
  where
    floorDescription = (string "The " *> many1 letter *> string " floor contains ") *> (componentList <* char '.')

zipperFromFloors :: [Floor] -> FacilityZipper
zipperFromFloors (x:xs) = (xs, x, [])

moveUp :: FacilityZipper -> Set.Set Component -> FacilityZipper
moveUp (x:above, cursor, below) cs = (above, newCurrent, newLower:below)
  where
    newLower = Set.difference cursor cs
    newCurrent = Set.union x cs

moveDown :: FacilityZipper -> Set.Set Component -> FacilityZipper
moveDown (above, cursor, x:below) cs = (newUpper:above, newCurrent, below)
  where
    newUpper = Set.difference cursor cs
    newCurrent = Set.union x cs

solve :: String -> IO ()
solve input = do
  let parsed = parse facilityDescription "" input
  case parsed of
    Left err -> print err
    Right floors -> print floors
