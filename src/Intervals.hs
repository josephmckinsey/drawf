module Intervals where

pyth :: (Floating a) => a -> a -> a
pyth x y = (x^2 + y^2)

dist :: (Floating a) => a -> a -> a
dist x y = sqrt $ pyth x y

bound :: (Ord a) => a -> a -> a -> a
bound below above =
  \x -> max below $ min above x

boundedBy :: (Ord a) => a -> a -> a -> Bool
boundedBy a b =
  if a <= b then \x -> (x <= b) && (x >= a)
  else \x -> (x <= a) && (x >= b)

scaleNum :: (Real a, Fractional b) => (a, a) -> (b, b) -> a -> b
scaleNum (lower, upper) (lower', upper') =
  let delta_0 = (realToFrac upper) - (realToFrac lower)
      delta_1 = upper' - lower'
  in \x -> ((realToFrac x) - (realToFrac lower)) / delta_0 * delta_1 + lower'

scaleToOne :: Real a => (a, a) -> a -> Double
scaleToOne initial = scaleNum initial (0, 1)

scaleToInt :: (Real a) => (a, a) -> (Int, Int) -> a -> Int
scaleToInt start (lower', upper') x = round $
  scaleNum start (realToFrac lower', realToFrac upper') x


scaleToDims :: (Real a) => (a, a) -> (Int, Int) -> (a, a) -> (Int, Int)
scaleToDims (startx, starty) (w, h) =
  \(x, y) -> (scaleToInt (0, startx) (0, w) x, scaleToInt (0, starty) (0, h) y)

scaleFromDims :: (Fractional b) => (Int, Int) -> (b, b) -> (Int, Int) -> (b, b)
scaleFromDims (w, h) (endx, endy) =
  \(x, y) -> (scaleNum (0, w) (0, endx) x, scaleNum (0, h) (0, endy) y)


scaleCorrection :: (Fractional b) => (Int, Int) -> (b, b) -> (Int, Int) -> (b, b)
scaleCorrection (w, h) (endx, endy) = \(x, y) ->
  (scaleNum (0, realToFrac w) (0, endx) ((realToFrac x) + 0.5),
   scaleNum (0, realToFrac h) (0, endy) ((realToFrac y) + 0.5))

app :: (a -> b) -> (a, a) -> (b, b)
app f (x, y) = (f x, f y)

flipCoord :: (a, b) -> (b, a)
flipCoord (a, b) = (b, a)
