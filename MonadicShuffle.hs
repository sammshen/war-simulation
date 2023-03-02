module MonadicShuffle where

import StateFunc
import System.Random

type RandFunc a = StateFunc StdGen a
  
removeAt :: Int -> [a] -> (a, [a])
removeAt i xs = let (l, v:r) = splitAt i xs in (v, l++r)

removeRandom :: [a] -> RandFunc (a, [a])
removeRandom xs = do
  i <- StateFunc $ uniformR (0, length xs - 1)
  pure $ removeAt i xs

shuffle :: [a] -> RandFunc [a]
shuffle [] = StateFunc ([],)
shuffle xs = do 
  (y, ys) <- removeRandom xs
  shuffled_ys <- shuffle ys
  pure (y:shuffled_ys)
