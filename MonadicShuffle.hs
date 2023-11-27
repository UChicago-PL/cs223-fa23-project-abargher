module MonadicShuffle where

import State
import System.Random

type Rand a = State StdGen a

removeAt :: Int -> [a] -> (a, [a])
removeAt i as =
  let
    enum = zip [0..] as
    noI = filter (\p -> fst p /= i) enum
  in
    case lookup i enum of
      Nothing -> error "index out of bounds"
      Just a -> (a, map snd noI)

removeRandom :: [a] -> Rand (a, [a])
removeRandom as = do
  g <- get
  let (i, g') = uniformR (0, length as - 1) g
  put g'
  pure (removeAt i as)

shuffle :: [a] -> Rand [a]
shuffle [] = pure []
shuffle as = do
  (a, as') <- removeRandom as
  (a :) <$> shuffle as'
  
