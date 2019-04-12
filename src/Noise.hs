module Noise where

-- State Monad
import Control.Monad.Trans.State
import System.Random
-- Sorted stuff
import Data.List as L
-- replicateM
import Control.Monad
import Linear.V2

getRandom :: Random a => State StdGen a
getRandom = state random

getRandomR :: Random a => (a, a) -> State StdGen a
getRandomR = state . randomR

avoid :: (Random a, Eq a) => a -> State StdGen a
avoid a = do
    x <- getRandom
    if (x == a)
    then (avoid a)
    else return x

exponential :: (Floating a, Random a, Eq a) =>
    a -> State StdGen a
exponential lambda = do
    x <- avoid 0
    return (-log x / lambda)

sortedRandom :: (Floating a, Random a, Eq a, Ord a) =>
    Int -> State StdGen a -> State StdGen [a]
sortedRandom i r = do
    l <- replicateM i r
    let sorted = L.sort l
    return sorted

cumulativeSum :: Num a => [a] -> [a]
cumulativeSum x = let y = zipWith (+) x (0:y) in y

cumulativeRandom :: Num a => Int -> State StdGen a -> State StdGen [a]
cumulativeRandom i r = fmap cumulativeSum (replicateM i r)

random2DUnit :: State StdGen (V2 Double)
random2DUnit = do
  theta <- getRandomR (0, 2*pi)
  return $ V2 (cos theta) (sin theta)

-- randomArray :: State StdGen (Vector )
