module RawShuffle where

import System.Random

removeAt :: Int -> [a] -> (a, [a])
removeAt i xs = let (l, v:r) = splitAt i xs in (v, l++r)

removeRandom :: [a] -> StdGen -> ((a, [a]), StdGen)
removeRandom xs g = 
  let
    (i, g') = uniformR (0, length xs - 1) g
    (y, ys) = removeAt i xs
  in
    ((y, ys), g')

shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle [] g = ([], g)
shuffle xs g = 
  let 
    ((y,ys), g') = removeRandom xs g
    (shuffled_ys, g'') = shuffle ys g'
  in 
    (y:shuffled_ys, g'')