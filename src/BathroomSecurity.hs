module BathroomSecurity (solve) where

import Data.Maybe (fromMaybe)
import Text.Parsec (parse)
import Text.Parsec.Char (char)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Combinator (many1, eof)

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
type ButtonGrid = (Button, (Int, Int) -> Maybe Button, Button -> Char)

buttonOnNormalGrid :: (Int, Int) -> Maybe Button
buttonOnNormalGrid (i, j) = if inBounds then Just (Button (i, j)) else Nothing
  where
    inBounds = i >= 0 && i <= 2 && j >= 0 && j <= 2

labelOnNormalGrid :: Button -> Char
labelOnNormalGrid (Button (i,j)) = head . show $ (i * 3) + j + 1

normalGrid :: ButtonGrid
normalGrid = (Button (1, 1), buttonOnNormalGrid, labelOnNormalGrid)

move :: ButtonGrid -> Button -> Direction -> Maybe Button
move (_,button,_) (Button (i,j)) U = button (i - 1, j)
move (_,button,_) (Button (i,j)) R = button (i, j + 1)
move (_,button,_) (Button (i,j)) D = button (i + 1, j)
move (_,button,_) (Button (i,j)) L = button (i, j - 1)

moveSafely :: ButtonGrid -> Button -> Direction -> Button
moveSafely g b d = fromMaybe b (move g b d)

bathroomCode :: ButtonGrid -> [[Direction]] -> [Button]
bathroomCode g@(bi,_,_) = bathroomCode' bi
  where
    bathroomCode' :: Button -> [[Direction]] -> [Button]
    bathroomCode' b [] = []
    bathroomCode' b (x:xs) = newButton : bathroomCode' b xs
      where
        newButton = foldl (moveSafely g) b x

formatCode :: ButtonGrid -> [Button] -> [Char]
formatCode (_,_,f) = map f

solve :: String -> IO ()
solve input = do
  let instructions = parse buttonInstructions "" input
  case instructions of
    Left err -> print err
    Right is -> do
      let codeForNormalGrid = formatCode normalGrid $ bathroomCode normalGrid is
      putStrLn codeForNormalGrid
