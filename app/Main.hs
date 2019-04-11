module Main where

import qualified Graphics.Image as I
import Graphics.Image (Image (..), Pixel(..), RGB, RPU(..))
import Lib
import System.Random
import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative
import Noise

import Intervals
--import Lines



--gradient :: (Double, Double) -> Pixel RGB Double
--gradient (x, y) = PixelRGB (x/2) (y/2) ((x + y) / 2)

--line :: (Double, Double) -> Pixel RGB Double
--line p =
--  let v = basicLine ((0.3,0),(0.9,0.5)) 0.05 p
--  in PixelRGB v v v

--sharpLine :: (Double, Double) -> Pixel RGB Double
--sharpLine p =
--  let v = realToFrac . ceiling $ basicLine ((0.1, 0.4), (0.9,0.6)) 0.02 p
--  in PixelRGB v v v

weirdGradient :: Double -> Pixel RGB Double
weirdGradient v
  | v > 0.4 = PixelRGB v 0 0
  | v > 0.1 = PixelRGB 0 (1 - v) (1 - v)
  | otherwise = PixelRGB v v v

--weirdLine :: (Double, Double) -> Pixel RGB Double
--weirdLine p =
--  weirdGradient $ basicLine ((0.1, 0.1), (0.9,0.9)) 0.05 p

cubicRamp :: Double -> Double
cubicRamp x = 3*x^2 - 2*x^3

cubicInterp :: Double -> Double -> Double -> Double
cubicInterp start end x =
  let w = cubicRamp (bound 0 1 x)
  in (1 - w)*start + w*end

sqrtInterp :: Double -> Double -> Double -> Double
sqrtInterp start end x =
  let w = sqrt (bound 0 1 x)
  in (1 - w)*start + w*end

squareInterp :: Double -> Double -> Double -> Double
squareInterp start end x =
  let w = (bound 0 1 x)^2
  in (1 - w)*start + w*end

weirderGradient :: (Double, Double) -> Pixel RGB Double
weirderGradient (over, out) =
  let g = (out**7) * (cubicInterp 0.1 0.6 over)
      b = out**(3*(squareInterp 1 3 over)) * (sqrtInterp 0 1 over)
  in PixelRGB 0 g b


--overOutLine :: (Double, Double) -> Pixel RGB Double
--overOutLine p =
--  weirderGradient $ lineOverOut ((0.1, 0.1), (0.9,0.9)) 0.1 p

generator = mkStdGen 2

randomCircle = circle (0.5, 0.5) (evalState (exponential 5) generator)

circle :: (Double, Double) -> Double -> (Double, Double) -> Double
circle (xo, yo) d  (x, y) =
    if ((dist (x - xo) (y - yo)) <= d)
    then 1
    else 0

logistic :: Double -> Double -> Double -> Double
logistic width location x = 1 / (1 + exp ((x - location) / width))

softCircle :: (Double, Double) -> Double -> (Double, Double) -> Double
softCircle (x0, y0) d (x, y) = logistic 0.0003 d (dist (x - x0) (y - y0))

randomDistList :: Int -> Double -> State StdGen [Double]
randomDistList i lambda = addManyRandom i $ exponential lambda

randomCircleList ::
    Int -> Double -> State StdGen [(Double, Double) -> Double]
randomCircleList i lambda = do
    dist <- randomDistList i lambda
    return $ fmap (circle (0.5, 0.5)) dist


minF :: Ord b => (a -> b) -> (a -> b) -> (a -> b)
minF f g x = min (f x) (g x)

minimumF :: (Foldable t, Ord b, Functor t) => t (a -> b) -> a -> b
minimumF xs x = Prelude.minimum $ fmap ($ x) xs

finalRandomCircles :: (Double, Double) -> Double
finalRandomCircles =
    evalState (fmap inverseMultList $ randomCircleList 50 100) generator

randomSoftCircles ::
    Int -> Double -> State StdGen [(Double, Double) -> Double]
randomSoftCircles i lambda = do
    dist <- randomDistList i lambda
    return $ fmap (softCircle (0.5, 0.5)) dist

randomSoftWobbly ::
    Int -> Double -> Double -> State StdGen [(Double, Double) -> Double]
randomSoftWobbly i lambda wobble = do
    dist <- fmap ZipList $ randomDistList i lambda
    units <- fmap ZipList $ replicateM i random2DUnit
    let shift = ((Vec2 (0.5, 0.5)) + ) . (scale (1/(wobble * lambda)))
        locations = fmap shift units
    return $ getZipList $ liftA2 softCircle (fmap out locations) dist

finalWobbly :: (Double, Double) -> Double
finalWobbly =
    evalState (fmap inverseMultList $ randomSoftWobbly 50 100 0.5) generator
    

finalSoftCircles :: (Double, Double) -> Double
finalSoftCircles =
    evalState (fmap inverseMultList $ randomSoftCircles 50 100) generator

inverseMult :: Num b => (a -> b) -> (a -> b) -> (a -> b)
inverseMult old new = \x -> (new x) * (1 - (old x))

inverseMultList ::  Num b => [a -> b] -> a -> b
inverseMultList xs = foldl inverseMult (const 0) xs

gray :: Double -> Pixel RGB Double
gray x = PixelRGB x x x

test_dim = 1280

mapFromIndices :: (Int, Int) -> (Double, Double)
mapFromIndices =
    flipCoord . (scaleCorrection (test_dim, test_dim) (1, 1))

testFunction :: ((Double, Double) -> Pixel RGB Double) -> Image RPU RGB Double
testFunction f = I.makeImageR RPU (test_dim, test_dim)
  (f . mapFromIndices)


testCirclea =  testFunction (gray . (circle (0.5, 0.5) 0.3))
testCircle = testFunction (gray . randomCircle)
testCircles = testFunction (gray . finalRandomCircles)
testSoftCircles = testFunction (gray . finalSoftCircles)
testWobblyCircles = testFunction (gray . finalWobbly)

main :: IO ()
main = do
  putStrLn "Working..."
  --let d = evalState (exponential 3) generator
  --print d
  I.writeImage "image/testWobbly.png" testWobblyCircles
