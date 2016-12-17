{-# LANGUAGE FlexibleContexts #-}

module Generators (solve) where

import Text.Parsec.Prim (Stream, ParsecT, parse, (<|>), try)
import Text.Parsec.Char (char, string, endOfLine, letter)
import Text.Parsec.Combinator (many1, sepBy, option, optional, eof, endBy)

type Element = String
data ComponentType = Generator | Microchip deriving Show
type Component = (Element, ComponentType)

component :: Stream s m Char => ParsecT s u m Component
component = (\e t -> (e, t)) <$> many1 letter <*> (generator <|> microchip)
  where
    generator = Generator <$ string " generator"
    microchip = Microchip <$ string "-compatible microchip"

componentList :: Stream s m Char => ParsecT s u m [Component]
componentList = ([] <$ string "nothing relevant") <|>
                (++) <$>
                (string "a " *> (component `sepBy` try (string ", a "))) <*>
                (option [] (pure <$> lastItem))
  where
    lastItem = optional (char ',') *> string " and a " *> component

facilityDescription :: Stream s m Char => ParsecT s u m [[Component]]
facilityDescription = floorDescription `endBy` endOfLine <* eof
  where
    floorDescription = (string "The " *> many1 letter *> string " floor contains ") *> (componentList <* char '.')

solve :: String -> IO ()
solve input = do
  let parsed = parse facilityDescription "" input
  case parsed of
    Left err -> print err
    Right floors -> print floors
