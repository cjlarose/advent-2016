{-# LANGUAGE FlexibleContexts #-}

module Generators (solve) where

import Text.Parsec.Prim (Stream, ParsecT, parse, (<|>), try)
import Text.Parsec.Char (char, string, endOfLine, letter)
import Text.Parsec.Combinator (many1, sepBy, option, optional)

type Element = String
data ComponentType = Generator | Microchip deriving Show
type Component = (Element, ComponentType)

component :: Stream s m Char => ParsecT s u m Component
component = (\e t -> (e, t)) <$> many1 letter <*> (generator <|> microchip)
  where
    generator = Generator <$ string " generator"
    microchip = Microchip <$ string "-compatible microchip"

componentList :: Stream s m Char => ParsecT s u m [Component]
componentList = (++) <$>
                (string "a " *> (component `sepBy` try (string ", a "))) <*>
                (option [] (pure <$> lastItem) <* char '.' <* endOfLine)
  where
    lastItem = optional (char ',') *> string " and a " *> component

solve :: String -> IO ()
solve = putStrLn
