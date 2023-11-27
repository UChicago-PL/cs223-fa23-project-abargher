module Stars where

import State
import System.Random
import Data.List
import           Graphics.Image (Image, Pixel(..), RGB, VU(VU))
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface (MArray)
import qualified Graphics.Image as Image

type Point = (Int, Int)
type Rand a = State StdGen a

distance :: Point -> Point -> Double
distance (i1,j1) (i2,j2) =
  let
    i1' = fromIntegral i1
    j1' = fromIntegral j1
    i2' = fromIntegral i2
    j2' = fromIntegral j2
  in
    sqrt ((i1' - i2')^2 + (j1' - j2')^2)

identifyCenters :: Point -> Double -> Double -> [Point] -> Rand [Point]
identifyCenters newPoint density threshold centers = do
    g <- get
    let (randVal, newGen) = uniformR (0,1) g
    put newGen
    if all (\p -> distance p newPoint <= threshold) centers && (randVal <= density) then
        pure [newPoint]
    else
        pure []

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


main :: IO ()
main = print "hello"