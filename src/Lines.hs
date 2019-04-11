module Lines where

import Intervals

type Line = (Double, Double, Double)
type Segment = ((Double, Double), (Double, Double))

makeLine :: Segment -> Line
makeLine ((x0, y0), (x1, y1)) = (a, b, c)
  where a = (y0 - y1)
        b = (x1 - x0)
        c = x0*(y1 - y0) - y0*(x1 - x0)

makeSegment :: (Real p) => (p, p) -> (p, p) -> Segment
makeSegment (x0', y0') (x1', y1') = ((x0, y0), (x1, y1))
  where x0 = realToFrac x0'
        y0 = realToFrac y0'
        x1 = realToFrac x1'
        y1 = realToFrac y1'

distToLine :: Line -> (Double, Double) -> Double
distToLine (a, b, c) (x, y) =
  (abs (a*x + b*y + c)) / (dist a b)

closestPointOnLine :: Line -> (Double, Double) -> (Double, Double)
closestPointOnLine (a, b, c) (x, y) =
  let xl = (b*(b*x - a*y) - a*c) / (pyth a b)
      yl = (a*(a*y - b*x) - b*c) / (pyth a b)
  in (xl, yl)

distOnEdge :: Segment -> (Double, Double) -> Double
distOnEdge ((x0, y0), (x1, y1)) =
  let b1 = boundedBy x0 x1
      b2 = boundedBy y0 y1
  in \(x,y) ->
    if (b1 x && b2 y) then 0
    else if (abs (x - x0) < abs (x - x1))
         then sqrt $ pyth (x - x0) (y - y0)
         else sqrt $ pyth (x - x1) (y - y1)

distToEdge :: Segment -> (Double, Double) -> Double
distToEdge s =
  let l = makeLine s
      (d1, d2) = (distToLine l, (distOnEdge s) . (closestPointOnLine l))
  in \p -> dist (d1 p) (d2 p)

scaleInSegment :: Segment -> (Double, Double) -> Double
scaleInSegment ((x0, y0), (x1, y1)) (x, y)
  | abs (x1 - x0) > 0.00001 = scaleNum (x0, x1) (0, 1) x
  | abs (y1 - y0) > 0.00001 = scaleNum (y0, y1) (0, 1) y
  | otherwise               = 0

distAlongEdge :: Segment -> (Double, Double) -> Double
distAlongEdge s =
  let l = makeLine s
  in (scaleInSegment s) . (closestPointOnLine l)

basicLine :: Segment -> Double -> (Double, Double) -> Double
basicLine s width = \p ->
  scaleNum (0, width) (1.0, 0.0) $
    bound 0 width (distToEdge s p)

lineOverOut :: Segment -> Double -> (Double, Double) -> (Double, Double)
lineOverOut s width = \p ->
  (distAlongEdge s p,
   scaleNum (0, width) (1.0, 0.0) $
   bound 0 width (distToEdge s p))

