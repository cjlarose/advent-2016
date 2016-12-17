module Main where

import System.Environment (getArgs)
import qualified Taxicab
import qualified BathroomSecurity
import qualified SquaresWithThreeSides
import qualified SecurityThroughObscurity
import qualified WarGames
import qualified SignalsAndNoise
import qualified InternetProtocolVersion7
import qualified TwoFactorAuthentication
import qualified ExplosivesInCyberspace
import qualified BalanceBots

solver :: Int -> (String -> IO ())
solver 1 = Taxicab.solve
solver 2 = BathroomSecurity.solve
solver 3 = SquaresWithThreeSides.solve
solver 4 = SecurityThroughObscurity.solve
solver 5 = WarGames.solve
solver 6 = SignalsAndNoise.solve
solver 7 = InternetProtocolVersion7.solve
solver 8 = TwoFactorAuthentication.solve
solver 9 = ExplosivesInCyberspace.solve
solver 10 = BalanceBots.solve

main :: IO ()
main = do
  args <- getArgs
  let problemNumber = head args
  contents <- readFile $ "inputs/" ++ problemNumber ++ ".txt"
  solver (read problemNumber) contents
