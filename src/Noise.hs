{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Noise where

import Control.Monad.Trans.State
import System.Random
import Data.List as L
import Control.Monad

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

sortedManyRandom :: (Floating a, Random a, Eq a, Ord a) =>
    Int -> State StdGen a -> State StdGen [a]
sortedManyRandom i r = do
    l <- replicateM i r
    let sorted = L.sort l
    return sorted

sumAlong :: Num a => [a] -> [a]
sumAlong x = let y = zipWith (+) x (0:y) in y

addManyRandom :: Num a => Int -> State StdGen a -> State StdGen [a]
addManyRandom i r = fmap sumAlong (replicateM i r)

newtype Vec2 a = Vec2 (a, a)
    deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

scale :: Num a => a -> Vec2 a -> Vec2 a
scale a v = fmap ((*) a) v

instance Num a => Num (Vec2 a)
    where (Vec2 (a, b)) + (Vec2 (x, y)) = Vec2 (a + x, b + y)
          (Vec2 (a, b)) * (Vec2 (x, y)) = Vec2 (a * x, b * y)
          fromInteger a = let x = fromInteger a in
            Vec2 (x, x)
          negate = fmap negate
          abs = fmap abs
          signum = fmap signum
          
--scale :: Num a => a -> Vec2 a -> Vec2 a
--scale a v = (fromInteger a) * v

dot :: Num a => Vec2 a -> Vec2 a -> a
dot v w = sum (v * w)

cross :: Num a => Vec2 a -> Vec2 a -> a
cross (Vec2 (a, b)) (Vec2 (c, d)) = a*d - b*c

out :: Vec2 a -> (a, a)
out (Vec2 (a, b)) = (a, b)

random2DUnit :: State StdGen (Vec2 Double)
random2DUnit = do
  theta <- getRandomR (0, 2*pi)
  return $ Vec2 (cos theta, sin theta)

-- randomArray :: State StdGen (Vector )
