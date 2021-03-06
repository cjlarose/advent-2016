{-# LANGUAGE FlexibleContexts #-}

module Generators (solve) where

import qualified Data.Set as Set
import Data.List (subsequences, foldl')
import Data.Maybe (fromJust)
import qualified Data.OrdPSQ as PSQ
import Text.Parsec.Prim (Stream, ParsecT, parse, (<|>), try)
import Text.Parsec.Char (char, string, endOfLine, letter)
import Text.Parsec.Combinator (many1, sepBy, option, optional, eof, endBy)

type Element = String
data ComponentType = Generator | Microchip deriving (Show, Ord, Eq)
type Component = (Element, ComponentType)
type Floor = Set.Set Component
type FacilityZipper = ([Floor], Floor, [Floor]) -- floors above, current floor, floors below
data ElevatorOption = Up | Down deriving Show

component :: Stream s m Char => ParsecT s u m Component
component = (\e t -> (e, t)) <$> many1 letter <*> (generator <|> microchip)
  where
    generator = Generator <$ string " generator"
    microchip = Microchip <$ string "-compatible microchip"

componentList :: Stream s m Char => ParsecT s u m Floor
componentList = (Set.empty <$ string "nothing relevant") <|>
                Set.union <$>
                (string "a " *> (Set.fromList <$> component `sepBy` try (string ", a "))) <*>
                option Set.empty (Set.singleton <$> lastItem)
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

move :: FacilityZipper -> ElevatorOption -> Set.Set Component -> FacilityZipper
move facility Up = moveUp facility
move facility Down = moveDown facility

safeFloor :: Floor -> Bool
safeFloor floor = not generatorOnFloor || null unshieldedChips
  where
    generatorElements = Set.fromList [e | (e, Generator) <- Set.toList floor]
    unshieldedChips = [e | (e, Microchip) <- Set.toList floor, e `Set.notMember` generatorElements]
    generatorOnFloor = not . null $ generatorElements

elevatorOptions :: FacilityZipper -> [ElevatorOption]
elevatorOptions (floorsAbove, _, floorsBelow) = concat $ zipWith (\a b -> if null b then [] else [a]) [Up, Down] [floorsAbove, floorsBelow]

potentialPayloads :: Floor -> [Set.Set Component]
potentialPayloads = map Set.fromList . filter (\x -> not (null x) && length x < 3) . subsequences . Set.toList

validNextSteps :: FacilityZipper -> [FacilityZipper]
validNextSteps facility@(floorsAbove, floor, floorsBelow) = do
  payload <- potentialPayloads floor
  direction <- elevatorOptions facility
  let newFacility@(_, newFloor, _) = move facility direction payload
  if safeFloor newFloor then return newFacility else []

readyForAssembly :: FacilityZipper -> Bool
readyForAssembly ([], _, floorsBelow) = not . any (not . Set.null) $ floorsBelow
readyForAssembly _ = False

ucs :: PSQ.OrdPSQ FacilityZipper Int [FacilityZipper] -> Set.Set FacilityZipper -> Int
ucs pqueue visited | readyForAssembly minK = minP
                   | otherwise = ucs newQueue (Set.insert minK visited)
  where
    (minK, minP, minV) = fromJust . PSQ.findMin $ pqueue
    updateQueue q node | node `Set.member` visited = q
                       | node `PSQ.member` q = PSQ.insert node (minP + 1) (minK:minV) $ PSQ.delete node q
                       | otherwise = PSQ.insert node (minP + 1) (minK:minV) q
    newQueue = foldl' updateQueue (PSQ.deleteMin pqueue) (validNextSteps minK)

shortestPathLength :: FacilityZipper -> Int
shortestPathLength start = ucs (PSQ.singleton start 0 []) Set.empty

solve :: String -> IO ()
solve input = do
  let parsed = parse facilityDescription "" input
  case parsed of
    Left err -> print err
    Right floors -> do
      let zipper = zipperFromFloors floors
      let numSteps = shortestPathLength zipper
      print numSteps

      let additionalComponents = Set.fromList [("elerium", Generator),
                                               ("elerium", Microchip),
                                               ("dilithium", Generator),
                                               ("dilithium", Microchip)]
      let newFirstFloor = Set.union additionalComponents (head floors)
      let newZipper = zipperFromFloors (newFirstFloor:tail floors)
      print $ shortestPathLength newZipper
