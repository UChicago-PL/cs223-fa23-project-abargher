module Stars where

import State
import System.Random
import Data.List
import Data.Ord
import GHC.Data.Maybe
import qualified Data.Map as Map
import           Graphics.Image (Image, Pixel(..), RGB, VU(VU))
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface (MArray)
import qualified Graphics.Image as Image
import UserInterface
import Numeric.Noise.Perlin

type Point = (Int, Int)
type Rand a = State StdGen a

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

chooseCenters :: [Point] -> Double -> [Point] -> Rand [Point]
chooseCenters [] _ centers = pure centers
chooseCenters (newPoint:as) density centers = do
    g <- get
    let (randVal, newGen) = uniformR (0 :: Double, 1 :: Double) g
    put newGen
    if randVal <= density then
      chooseCenters as density (newPoint : centers)
    else
      chooseCenters as density centers

black = Image.PixelY 0.0
white = Image.PixelY 1.0

getPixels :: Color -> [Point] -> [(Point, Image.Pixel RGBA Double)] -> [(Point, Pixel RGBA Double)]
getPixels bgColor locs lights =
  let
    background = Map.fromAscList $ map (, colorToPixel bgColor 1.0) locs
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

starSorter :: (Double, (Point, Double)) -> (Double, (Point, Double)) -> Ordering
starSorter (_, (p1, lum1)) (_, (p2, lum2)) = compare (p1, Down lum1) (p2, Down lum2)

avg :: Fractional a => [a] -> a
avg ns = sum ns / fromIntegral (length ns)

weightedAvg :: (Ord a, Fractional a) => [a] -> a
weightedAvg ns =
  let
    listLength = length ns
    enum = [1..] :: [Int]
    toN = (fromIntegral listLength * (fromIntegral listLength + 1) / 2)
  in
    sum (zipWith (*) (sort ns) (map fromIntegral enum)) / toN

avgDupsByFst :: (Ord a, Ord b, Fractional b) => [(c, (a, b))] -> [(c, (a, b))]
avgDupsByFst = map (\ls -> (fst (head ls), ((fst . snd) (head ls), weightedAvg (map (snd . snd) ls)))) . groupBy (\(_, (a1, _)) (_, (a2, _)) -> a1 == a2)

randPercent :: Int -> Point -> (Point, Double)
randPercent seed (x, y) =
  let
    perlinNoise = perlin seed 5 0.05 0.5
    perlinRes = noiseValue perlinNoise (fromIntegral x, fromIntegral y, 0)
  in
    ((x, y), (perlinRes + 1) * 0.5)

buildImage :: FilePath -> [Point] -> [Point] -> Specs -> Rand (IO ())
buildImage path locs centers (Specs { width = width
                                    , height = height
                                    , starSizeRange = radRange
                                    , bgColor = bgColor
                                    , starColors = (c1, c2)
                                    }) = do
  g <- get
  let (filledAll, g') = runState (mapM (buildNeighborhood width height radRange) centers) g
  let (seed, g'') = uniformR (1, 1000000) g'
  let centerColors = map (randPercent seed) centers
  let filled = zip centerColors filledAll
  let (lums, g''') = runState (mapM (\((center, col), lp) ->  mapM (luminance center) lp) filled) g''
  put g'''

  let lums' = map (filter (\(_, lum) -> lum > 0.01)) lums
  let lumsAndCol = zip (map snd centerColors) lums'
  let pixelsWithCol = concatMap (\(c, lp) -> map (c,) lp) lumsAndCol  -- [color, (position, lum)]
  let preLights = avgDupsByFst $ sortBy starSorter pixelsWithCol
  let lights = map (\(perc, (center, lum)) -> (center, blend (colorToPixel bgColor 1) (colorToPixel (gradient c1 c2 perc) lum))) preLights
  let pixels = splitEvery width $ map snd $ getPixels bgColor locs lights

  let img :: Image VU RGBA Double = Image.fromListsR VU pixels
  pure $ Image.writeImage path img

gaussian :: Double -> Double -> Double -> Double
gaussian mean variance x =
  exp ((-1) * ((x - mean) ** 2) / (2 * variance)) / sqrt (2 * pi * variance)

luminance :: Point -> Point -> Rand (Point, Double)
luminance center point = do
  let actualDistance = distance point center
  if actualDistance == 0 then
    pure (point, 1.0)
  else do
    g <- get
    let (randVal, newGen) = uniformR (0 :: Double, actualDistance :: Double) g
    put newGen
    let randomDistance = 0.9 * actualDistance + sqrt randVal
    let gaussianLuminance = gaussian gaussianMean gaussianVariance randomDistance
    let regularizedLuminance = (gaussianLuminance / gaussian gaussianMean gaussianVariance 0) / sqrt (avg [randVal, actualDistance])
    pure (point, regularizedLuminance)

-- Borrowed right from https://stackoverflow.com/a/16109302
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

rmdupsBy :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
rmdupsBy comp = map head . groupBy comp . sort

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

gaussianMean = 0
gaussianVariance = 100
distanceDampeningCoefficient = 2

blend :: Image.Pixel RGBA Double -> Image.Pixel RGBA Double -> Image.Pixel RGBA Double
-- First Color is background, second is foreground
blend (Image.PixelRGBA r1 g1 b1 _) (Image.PixelRGBA r2 g2 b2 a2) =
  colorToPixel (combine r1 r2 a2, combine g1 g2 a2, combine b1 b2 a2, 1) 1 where
    combine :: Num a => a -> a -> a -> a
    combine bg fg alpha = alpha * fg + (1-alpha) * bg

gradient :: Color -> Color -> Double -> Color
gradient (r1, g1, b1, _) (r2, g2, b2, _) percent =
  let
    rDiffPct = (r2 - r1) * percent
    gDiffPct = (g2 - g1) * percent
    bDiffPct = (b2 - b1) * percent
  in
    (r1 + rDiffPct, g1 + gDiffPct, b1 + bDiffPct, 1)


colorToPixel :: Color -> Double -> Pixel RGBA Double
colorToPixel (r1, g1, b1, _) = PixelRGBA r1 g1 b1

main :: IO ()
main = do
  stdGen <- initStdGen
  out <- runMaybeT getParameters
  case out of
    Nothing -> putStrLn "Invalid argument. Please try again."
    Just specs@(Specs { width = imgWidth, height = imgHeight, fileName = fname}) -> do
      let locs = [(i, j) | i <- [0..imgHeight-1], j <- [0..imgWidth-1]]
      let centers = evalState (chooseCenters locs 0.0004 []) stdGen
      evalState (buildImage fname locs centers specs) stdGen
