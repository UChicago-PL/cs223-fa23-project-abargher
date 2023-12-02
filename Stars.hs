module Stars where

import State
import System.Random
import Data.List
import qualified Data.Map as Map
import           Graphics.Image (Image, Pixel(..), RGB, VU(VU))
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface (MArray)
import qualified Graphics.Image as Image
import MonadicShuffle (Rand)
type Point = (Int, Int)

gaussianMean = 0
gaussianVariance = 100
distanceDampeningCoefficient = 2

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
    if randVal <= density then
      identifyCenters as density (newPoint : centers)
    else
      identifyCenters as density centers

black = Image.PixelY 0.0
white = Image.PixelY 1.0

getPixels :: [Point] -> [(Point, Image.Pixel Y Double)] -> [(Point, Pixel Y Double)]
getPixels locs lights =
  let
    background = Map.fromAscList $ map (, black) locs
    lights' = Map.fromAscList lights
  in
    Map.toAscList $ Map.unionWith const lights' background


withinBounds :: Int -> Int -> Point -> Bool
withinBounds width height (r, c) =
  (r >= 0 && r < height) && (c >= 0 && c < width)


cartesianToImg :: Int -> Int -> Point -> Point
cartesianToImg width height (x,y) = (-y - height `div` 2, x + width `div` 2)

imgToCartesian :: Int -> Int -> Point -> Point
imgToCartesian width height (r, c) = (c - width `div` 2, (-r) - height `div` 2)

cartesianToImages :: Int -> Int -> [Point] -> [Point]
cartesianToImages width height cartesianPoints =
  filter (withinBounds width height) (map (cartesianToImg width height) cartesianPoints)

imgToCartesians :: Int -> Int -> [Point] -> [Point]
imgToCartesians width height imgPoints =
  filter (\(x,y) -> (x >= -width `div` 2) && (x <= -width `div` 2) && (y >= -height `div` 2) && (y <= height `div` 2))
  (map (imgToCartesian width height) imgPoints)


buildImage :: FilePath -> Int -> Int  -> [Point] -> [Point] -> Rand (IO ())
buildImage path width height locs centers = do
  g <- get
  -- range of star radii - refactor into an input
  let lower = 2
  let upper = 10
  let (filledAll, g') = runState (mapM (buildNeighborhood width height (lower, upper)) centers) g

  -- let filled = zip centers $ map (filter (withinBounds width height)) filledAll
  let filled = zip centers filledAll

  let (lums, g'') = runState (mapM (\(center, lp) -> mapM (luminance center) lp) filled) g'
  let lights = map (Image.PixelY <$>) $ sort $ concat lums
  let pixels = splitEvery width $ map snd $ getPixels locs lights

  let img :: Image VU Y Double = Image.fromListsR VU pixels
  pure $ Image.writeImage path img

gaussian :: Double -> Double -> Double -> Double
gaussian mean variance x =
  exp ((-1) * ((x - mean) ** 2) / (2 * variance)) / sqrt (2 * pi * variance)

luminance :: Point -> Point -> Rand (Point, Double)
luminance center point = do
  g <- get
  let (randVal, newGen) = uniformR (0 :: Double, 1 :: Double) g
  put newGen
  let actualDistance = distance point center
  if actualDistance == 0 then
    pure (point, 1.0)
  else do
    let gaussianLuminance = gaussian gaussianMean gaussianVariance (actualDistance * distanceDampeningCoefficient + randVal)
    let regularizedLuminance = gaussianLuminance / gaussian gaussianMean gaussianVariance 0
    pure (point, regularizedLuminance)

-- Borrowed right from https://stackoverflow.com/a/16109302
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

mirroredPoints :: Point -> [Point]
mirroredPoints (y, x) =
  let
    smallerY = min y (-y)
    biggerY = max y (-y)
    rightVert = [(x, i) | i <- [smallerY..biggerY]]
    topHoriz = [(i, x) | i <- [smallerY..biggerY]]
    leftVert = [(-x, i) | i <- [smallerY..biggerY]]
    bottomHoriz = [(i, -x) | i <- [smallerY..biggerY]]
  in
    (rightVert ++ topHoriz ++ leftVert ++ bottomHoriz)

tupleAdd :: Num a => (a, a) -> (a, a) -> (a, a)
tupleAdd (a, b) (x, y) = (a + x, b + y)

-- Using the midpoint circle algorithm to generate a "circle" in the grid
generateCircle :: Point -> Int -> [Point]
generateCircle center radius =
  map (tupleAdd center) $ rmdups $ generateCircle' radius 0 (1 - radius)
  where
  generateCircle' :: Int -> Int -> Int -> [Point]
  generateCircle' x y p
    | y >= x = []
    | otherwise = mirroredPoints (x, y) ++
      if p <= 0 then
        generateCircle' x (y + 1) (p + (2 * y) + 1)
      else
        generateCircle' (x - 1) (y + 1) (p + (2 * (y - x) + 1))

buildNeighborhood :: Int -> Int -> (Int, Int) -> Point -> Rand [Point]
buildNeighborhood width height radRange p = do
  g <- get
  let (radius, g') = uniformR radRange g
  put g'
  let p' = imgToCartesian width height p
  let cartesianPts = generateCircle p' radius
  let imgPts = cartesianToImages width height cartesianPts
  pure imgPts


imgWidth = 5120
imgHeight = 2880

main :: IO ()
main = do
  stdGen <- initStdGen
  let locs = [(i, j) | i <- [0..imgHeight-1], j <- [0..imgWidth-1]]
  let centers = evalState (identifyCenters locs 0.001 []) stdGen
  -- print centers
  evalState (buildImage "test-1.png" imgWidth imgHeight locs centers) stdGen
  -- let (point, newLuminance) = evalState (luminance (0,0) (3,0)) stdGen
  -- let as = evalState (luminance (0,0) (1,0)) stdGen

  -- print $ sort $ mirroredPoints (2,1)