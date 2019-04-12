module Lib
    where

import Graphics.Image (Pixel(..), RGB(..))
import Control.Applicative
import Linear.Metric

bound :: (Ord a) => a -> a -> a -> a
bound below above =
  \x -> max below $ min above x

boundedBy :: (Ord a) => a -> a -> a -> Bool
boundedBy a b =
  if a <= b then \x -> (x <= b) && (x >= a)
  else \x -> (x <= a) && (x >= b)

-- rewrite to work on all floats

cubicRamp :: Num a => a -> a
cubicRamp x = 3*x^2 - 2*x^3

interp :: Num a => (t -> a) -> a -> a -> t -> a
interp f start end x = (1 - (f x))*start + (f x)*end

cubicInterp :: (Num a, Ord a) => a -> a -> a -> a
cubicInterp = interp $ cubicRamp . (bound 0 1)

sqrtInterp :: (Floating a, Ord a) => a -> a -> a -> a
sqrtInterp = interp $ sqrt . (bound 0 1)

squareInterp :: (Num a, Ord a) => a -> a -> a -> a
squareInterp = interp $ (^ 2) . (bound 0 1)

indicatorLess :: (Ord a, Num p) => a -> a -> p
indicatorLess i x =
    if x <= i
    then 1
    else 0

-- rewrite to be more general and operate on vec2
circle :: (Ord a, Floating a, Metric f, Num c) => a -> f a -> c
circle d = indicatorLess d . norm

-- sigmoid sort of function tha moves to one
logistic :: Floating a => a -> a -> a -> a
logistic w l x = 1 / (1 + exp ((x - l) / w))

softCircle :: (Floating c, Metric f) => c -> f c -> c
softCircle d = (logistic 0.0003 d) . norm

-- General function combinations
minF :: Ord b => (a -> b) -> (a -> b) -> (a -> b)
minF f g x = min (f x) (g x)

minimumF :: (Foldable t, Ord b, Functor t) => t (a -> b) -> a -> b
minimumF xs x = Prelude.minimum $ fmap ($ x) xs

-- Num function (fmap and lift) implemented elsewhere
instance Num a => Num ((->) r a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = fmap abs
    negate = fmap negate
    signum = fmap signum
    fromInteger = const . fromInteger

-- short for inverse multiplication (works on functions)
imult :: Num a => a -> a -> a
imult old new = new * (1 - old)

imultList :: Num a => [a] -> a
imultList = foldl imult (fromInteger 0)

gray :: Double -> Pixel RGB Double
gray x = PixelRGB x x x
