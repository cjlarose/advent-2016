{-# LANGUAGE FlexibleContexts #-}

module BalanceBots (solve) where

import Text.Parsec.Prim (Stream, ParsecT, parse, (<|>))
import Text.Parsec.Char (digit, string, endOfLine)
import Text.Parsec.Combinator (many1, endBy, eof)

newtype Bot = Bot Int deriving Show
data Destination = ToOutput Int | ToBot Bot deriving Show
data Stmt = ValueStmt Bot Int | RuleStmt Bot (Destination, Destination) deriving Show

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

solve :: String -> IO ()
solve input = do
  let parsed = parse instructionList "" input
  case parsed of
    Left err -> print err
    Right stmts -> print stmts