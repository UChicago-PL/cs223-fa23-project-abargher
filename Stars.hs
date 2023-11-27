module Stars where

import Data.List
import           Graphics.Image (Image, Pixel(..), RGB, VU(VU))
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface (MArray)
import qualified Graphics.Image as Image

type Point = (Int, Int)

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