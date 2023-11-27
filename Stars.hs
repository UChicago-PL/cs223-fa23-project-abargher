module Stars where

import State

import System.Random

import Data.List
import           Graphics.Image (Image, Pixel(..), RGB, VU(VU))
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface (MArray)
import qualified Graphics.Image as Image
import MonadicShuffle
type Point = (Int, Int)

-- type Rand a = State StdGen a

-- Taken directly from source at 
-- https://hackage.haskell.org/package/list-grouping-0.1.1/docs/Data-List-Grouping.html#v%3asplitEvery
-- source code: https://hackage.haskell.org/package/list-grouping-0.1.1/docs/src/Data-List-Grouping.html#splitEvery
-- Only needed this function, not worth installing a whole additional package.
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
     where (as,bs) = splitAt n xs

distance :: Point -> Point -> Double
distance (i1,j1) (i2,j2) =
  let
    i1' = fromIntegral i1
    j1' = fromIntegral j1
    i2' = fromIntegral i2
    j2' = fromIntegral j2
  in
    sqrt ((i1' - i2')^2 + (j1' - j2')^2)

identifyCenters :: [Point] -> Double -> [Point] -> Rand [Point]
identifyCenters [] _ centers = pure centers
identifyCenters (newPoint:as) density centers = do
    g <- get
    let (randVal, newGen) = uniformR (0 :: Double, 1 :: Double) g
    put newGen
    -- if all (\p -> distance p newPoint >= threshold) centers && (randVal <= density) then
    if randVal <= density then
      identifyCenters as density (newPoint : centers)
    else
      identifyCenters as density centers

black = Image.PixelY 0.0
white = Image.PixelY 1.0

getPixels :: [Point] -> [Point] -> [Pixel Y Double]
getPixels [] _ = []
getPixels rest [] = replicate (length rest) black
getPixels (p:ps) cs@(c:cs') =
  if p == c then
    white : getPixels ps cs'
  else
    black : getPixels ps cs

buildImage :: FilePath -> Int -> Int -> [Point] -> IO ()
buildImage path width height centers =
  let
    black = Image.PixelY 0.0
    white = Image.PixelY 1.0
    -- locs = groupBy (\(a, _) (b, _) -> a == b)
    --         [(i, j) | i <- [0..height-1], j <- [0..width-1]]
    locs = [(i, j) | i <- [0..height-1], j <- [0..width-1]]
    pixels = splitEvery width $ getPixels locs (sort centers)
    img :: Image VU Y Double = Image.fromListsR VU pixels
  in
    Image.writeImage path img


height = 1000
width = 1000

main :: IO ()
main = do
  stdGen <- initStdGen
  -- let centers = evalState (identifyCenters (evalState (shuffle [(i, j) | i <- [0..height-1], j <- [0..width-1]]) stdGen) 0.1 8.0 []) stdGen
  let centers = evalState (identifyCenters [(i, j) | i <- [0..height-1], j <- [0..width-1]] 0.001 []) stdGen
  buildImage "test-1.png" width height centers