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

distance :: Point -> Point -> Double
distance (i1,j1) (i2,j2) =
  let
    i1' = fromIntegral i1
    j1' = fromIntegral j1
    i2' = fromIntegral i2
    j2' = fromIntegral j2
  in
    sqrt ((i1' - i2')^2 + (j1' - j2')^2)

identifyCenters :: [Point] -> Double -> Double -> [Point] -> Rand [Point]
identifyCenters [] _ _ centers = pure centers
identifyCenters (newPoint:as) density threshold centers = do
    g <- get
    let (randVal, newGen) = uniformR (0 :: Double, 1 :: Double) g
    put newGen
    if all (\p -> distance p newPoint >= threshold) centers && (randVal <= density) then
      identifyCenters as density threshold (newPoint : centers)
    else
      identifyCenters as density threshold centers

getPixels :: [[Point]] -> [Point] -> [[Pixel Y Double]]
getPixels locs centers =
  let
    black = Image.PixelY 0.0
    white = Image.PixelY 1.0
  in
    map (map (\pr -> if pr `elem` centers then white else black)) locs

buildImage :: FilePath -> Int -> Int -> [Point] -> IO ()
buildImage path width height centers =
  let
    black = Image.PixelY 0.0
    white = Image.PixelY 1.0
    locs = groupBy (\(a, _) (b, _) -> a == b)
            [(i, j) | i <- [0..height-1], j <- [0..width-1]]
    pixels = getPixels locs centers
    img :: Image VU Y Double = Image.fromListsR VU pixels
  in
    Image.writeImage path img


height = 100
width = 100

main :: IO ()
main = do
  stdGen <- initStdGen
  let centers = evalState (identifyCenters (evalState (shuffle [(i, j) | i <- [0..height-1], j <- [0..width-1]]) stdGen) 0.1 8.0 []) stdGen
  buildImage "test-1.png" width height centers