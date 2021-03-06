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
import qualified Generators
import qualified Monorail
import qualified CubicleMaze
import qualified OneTimePad
import qualified TimingIsEverything
import qualified DragonChecksum
import qualified TwoStepsForward
import qualified Rogue
import qualified ElfNamedJoseph
import qualified FirewallRules
import qualified ScrambledLetters

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
solver 11 = Generators.solve
solver 12 = Monorail.solve
solver 13 = CubicleMaze.solve
solver 14 = OneTimePad.solve
solver 15 = TimingIsEverything.solve
solver 16 = DragonChecksum.solve
solver 17 = TwoStepsForward.solve
solver 18 = Rogue.solve
solver 19 = ElfNamedJoseph.solve
solver 20 = FirewallRules.solve
solver 21 = ScrambledLetters.solve

main :: IO ()
main = do
  args <- getArgs
  let problemNumber = head args
  contents <- readFile $ "inputs/" ++ problemNumber ++ ".txt"
  solver (read problemNumber) contents
