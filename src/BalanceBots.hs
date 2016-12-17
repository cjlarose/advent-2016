{-# LANGUAGE FlexibleContexts #-}

module BalanceBots (solve) where

import qualified Data.Map.Strict as Map
import Data.Foldable (foldl')
import Data.Maybe (listToMaybe, isJust, fromJust)
import Control.Monad.State (State, state, runState)
import Control.Monad.Loops (untilM_)
import Text.Parsec.Prim (Stream, ParsecT, parse, (<|>))
import Text.Parsec.Char (digit, string, endOfLine)
import Text.Parsec.Combinator (many1, endBy, eof)

newtype Bot = Bot Int deriving (Show, Ord, Eq)
data Destination = ToOutput Int | ToBot Bot deriving (Show, Ord, Eq)
data Stmt = ValueStmt Bot Int | RuleStmt Bot (Destination, Destination) deriving Show
type Rules = Map.Map Bot (Destination, Destination)
type Possession = Map.Map Destination [Int]
type Factory = (Rules, Possession)

intLiteral :: Stream s m Char => ParsecT s u m Int
intLiteral = read <$> many1 digit

bot :: Stream s m Char => ParsecT s u m Bot
bot = string "bot " *> (Bot <$> intLiteral)

valueStmt :: Stream s m Char => ParsecT s u m Stmt
valueStmt = flip ValueStmt <$>
            (string "value " *> intLiteral) <*>
            (string " goes to " *> bot)

valueDestination :: Stream s m Char => ParsecT s u m Destination
valueDestination = (ToBot <$> bot) <|> (ToOutput <$> output)
  where
    output = string "output " *> intLiteral

ruleStmt :: Stream s m Char => ParsecT s u m Stmt
ruleStmt = (\bot lo hi -> RuleStmt bot (lo, hi)) <$>
           bot <*>
           (string " gives low to " *> valueDestination) <*>
           (string " and high to " *> valueDestination)

instructionList :: Stream s m Char => ParsecT s u m [Stmt]
instructionList = endBy (valueStmt <|> ruleStmt) endOfLine <* eof

initialState :: [Stmt] -> Factory
initialState stmts = (rules, possessions)
  where
    rules = Map.fromList [(b, ds) | (RuleStmt b ds) <- stmts]
    possessions = Map.fromListWith (++) [(ToBot b, [v]) | (ValueStmt b v) <- stmts]

runFactory' :: Factory -> Factory
runFactory' (rules, possessions) = (rules, newPossessions)
  where
    readyDestinations :: [(Destination, [Int])]
    readyDestinations = Map.toList $ Map.filter ((== 2) . length) possessions

    readyBots :: [(Bot, [Int])]
    readyBots = [(b, v) | ((ToBot b), v) <- readyDestinations]

    updatesForBot :: (Bot, [Int]) -> [(Destination, Int)]
    updatesForBot (b, vs) = let (dl, dh) = rules Map.! b in [(dl, minimum vs), (dh, maximum vs)]

    additions :: [(Destination, Int)]
    additions = concatMap updatesForBot readyBots

    possessionsWithAdditions = foldl' (\m (k, v) -> Map.insertWith (++) k [v] m) possessions additions

    newPossessions = foldl' (flip Map.delete) possessionsWithAdditions (map (ToBot . fst) readyBots)

runFactory :: State Factory ()
runFactory = state (\factory -> ((), runFactory' factory))

specialBot :: State Factory (Maybe Bot)
specialBot = state (\factory@(_, possessions) -> (specialBot' possessions, factory))
  where
    specialList xs = 61 `elem` xs && 17 `elem` xs
    specialBot' ps = listToMaybe [b | (ToBot b, _) <- Map.toList $ Map.filter specialList ps]

runFactoryUntilSpecialBotShowsHimself :: State Factory Bot
runFactoryUntilSpecialBotShowsHimself = do
  untilM_ runFactory (isJust <$> specialBot)
  specialDude <- specialBot
  return (fromJust specialDude)

solve :: String -> IO ()
solve input = do
  let parsed = parse instructionList "" input
  case parsed of
    Left err -> print err
    Right stmts -> do
      let factory = initialState stmts
      let ((Bot b), _) = runState runFactoryUntilSpecialBotShowsHimself factory
      print b
