module BathroomSecurity (solve) where

import Data.Char (toUpper)
import Numeric (showHex)
import Data.Maybe (fromMaybe)
import Text.Parsec (parse)
import Text.Parsec.Char (char)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Combinator (many1, sepBy, endBy, eof)

data Direction = U | R | D | L deriving Show

eol = char '\n'

up =    char 'U' >> pure U
right = char 'R' >> pure R
down =  char 'D' >> pure D
left =  char 'L' >> pure L

direction = up <|> right <|> down <|> left

directionLine = do
  line <- many1 direction
  eol
  return line

buttonInstructions = do
  res <- many1 directionLine
  eof
  return res

newtype Button = Button (Int, Int) deriving Show

button :: (Int, Int) -> Maybe Button
button (i, j) = if inBounds then Just (Button (i, j)) else Nothing
  where
    inBounds = i >= 0 && i <= 2 && j >= 0 && j <= 2

move :: Button -> Direction -> Maybe Button
move (Button (i,j)) U = button (i - 1, j)
move (Button (i,j)) R = button (i, j + 1)
move (Button (i,j)) D = button (i + 1, j)
move (Button (i,j)) L = button (i, j - 1)

moveSafely :: Button -> Direction -> Button
moveSafely b d = fromMaybe b (move b d)

bathroomCode :: Button -> [[Direction]] -> [Button]
bathroomCode b [] = []
bathroomCode b (x:xs) = newButton : bathroomCode newButton xs
  where
    newButton = foldl moveSafely b x

buttonLabel :: Button -> Char
buttonLabel (Button (i,j)) = toUpper . head $ showHex ((i * 3) + j + 1) ""

solve :: String -> IO ()
solve input = do
  let instructions = parse buttonInstructions "" input
  case instructions of
    Left err -> print err
    Right is -> do
      let initialButton = Button (1, 1)
      let code = map buttonLabel $ bathroomCode initialButton is
      putStrLn code
