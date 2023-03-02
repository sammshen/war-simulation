module RandomWars where

import Cards
import MonadicShuffle
import StateFunc
import System.Random

maxTurns = 120

howLongDoWarsLast :: Int
howLongDoWarsLast = 28

main :: IO ()
main = do
  stdGen <- initStdGen
  let numGames = 1000
  let ns = evalStateFunc (simulateGames numGames) stdGen
  print (sum ns `div` numGames)

simulateGames :: Int -> RandFunc [Int]
simulateGames 0 = StateFunc ([],)
simulateGames numGames = do 
  let cards = fullDeck True
  shuffled_cards <- shuffle cards
  let (p1_cards, p2_cards) = splitAt 26 shuffled_cards
  let n = simulateGame p1_cards p2_cards [] 1
  ns <- simulateGames (numGames - 1)
  pure (n:ns)

simulateGame :: [Card a] -> [Card a] -> [Card a] -> Int -> Int
simulateGame (p1_top@(Card v1 _) : p1_rest) 
             (p2_top@(Card v2 _) : p2_rest) 
             pile turns
  | turns > maxTurns 
    = turns
  | v1 < v2
    = simulateGame p1_rest (p2_rest ++ [p1_top] ++ pile) [] (turns+1)
  | v1 > v2
    = simulateGame (p2_rest ++ [p2_top] ++ pile) p2_rest [] (turns+1)
  | otherwise
    = simulateGame p1_rest p2_rest (p1_top:p2_top:pile) (turns+1)
simulateGame _ _ _ turns = turns 
--if player runs out of cards during war, terminates