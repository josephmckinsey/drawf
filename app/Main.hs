module Main where

import qualified Graphics.Image as I
import Graphics.Image (Image (..), Pixel(..), RGB, RPU(..), VU(..))

import System.Random
import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative
import Linear.V2

import Noise
import Lib

generator = mkStdGen 2

-- Takes number of circles, the inverse of the expected spacing
-- and a wobbling parameter
randomSoftWobbly :: Int -> Double -> Double ->
    State StdGen [V2 Double -> Double]
randomSoftWobbly i lambda wobble = do
    dist <- fmap ZipList $ cumulativeRandom i (exponential lambda)
    units <- fmap ZipList $ replicateM i random2DUnit
    -- Move circles to center of image
    let transform x = (x / (V2 (wobble*lambda) (wobble*lambda))) +
              (V2 0.5 0.5)
        locations = fmap transform units
        circles = fmap softCircle dist
    return . getZipList $
        liftA2 (\c l v -> c (v - l)) circles locations

realCircles :: V2 Double-> Double
realCircles =
    evalState (fmap imultList $ randomSoftWobbly 50 100 0.1) generator

plot :: V2 Double -> Pixel RGB Double
plot = gray . realCircles

testDim :: Num a => V2 a
testDim = V2 1280 1280

defaultTransform :: (Int, Int) -> V2 Double
defaultTransform (i, j) =
    let v = testDim * (V2 0 1) +
         (V2 (realToFrac j) (- (realToFrac i)))
    in (v + (V2 0.5 0.5)) / testDim

final :: (Int, Int) -> Pixel RGB Double
final = plot . defaultTransform

image :: ((Int, Int) -> Pixel RGB Double) -> Image RPU RGB Double
image x = I.makeImage (out testDim) x
    where out (V2 a b) = (a, b)

main :: IO ()
main = do
  putStrLn "Working..."
  origin <- I.readImage' "image/masked.jpg" :: IO (Image RPU RGB Double)
  I.writeImage "image/example.png" $ I.zipWith (*) origin (image final)
