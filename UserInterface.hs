module UserInterface where

import System.IO
import Text.ParserCombinators.ReadP
import GHC.Data.Maybe
import Data.Char
import Data.List.Split
import Control.Monad

type Color = (Double, Double, Double, Double)

data Specs = Specs { width :: Int
                   , height :: Int
                   , starSizeRange :: (Int, Int)
                   , starColors :: (Color, Color)
                   , bgColor :: Color
                   }
  deriving Show

resolutionMap :: [(Int, Int)]
resolutionMap = 
  [
    (500, 500),
    (1000, 1000),
    (1920, 1080),
    (2560, 1440),
    (3840, 2160),
    (5120, 2880),
    (1920, 1200),
    (2560, 1600),
    (3840, 2400),
    (5120, 3200)
  ]

resolutionOptions :: String
resolutionOptions = 
  "Choose an image resolution:\n\
  \  1) 500x500 (1:1)\n\
  \  2) 1000x1000 (1:1)\n\
  \  3) 1920x1080 (16:9)\n\
  \  4) 2560x1440 (16:9)\n\
  \  5) 3840x2160 (16:9)\n\
  \  6) 5120x2880 (16:9)\n\
  \  7) 1920x1200 (16:10)\n\
  \  8) 2560x1600 (16:10)\n\
  \  9) 3840x2400 (16:10)\n\
  \  10) 5120x3200 (16:10)\n\
  \  11) custom resolution\n\
  \\n\
  \Type a number (1-11): \
  \"

getResolution :: MaybeT IO (Int, Int)
getResolution = do
  line' <- liftMaybeT getLine
  let line = filter (not . isSpace) line'
  guard (all isDigit line)
  let choice = read line :: Int
  guard (choice > 0 && choice <= 11)
  if choice == 11 then do
    prompt "Custom resolution (e.g. 1200x1200)? "
    custom <- liftMaybeT getLine
    let custom' = splitOn "x" $ filter (not . isSpace) custom
    guard (length custom' == 2)
    let width = read $ head custom'
    let height = read $ last custom'
    pure (width, height)
  else do
    pure $ resolutionMap !! choice

getRadius :: MaybeT IO Int
getRadius = do
  line <- liftMaybeT getLine
  guard (all isDigit line)
  pure $ read line

getColor :: MaybeT IO Color
getColor = do
  line <- liftMaybeT getLine
  let line' = splitOn "," $ filter (not . isSpace) line
  guard (length line' == 3)
  let cols = map read line'
  let [r, g, b] = cols
  pure (r, g, b, 1.0)

-- getResolution :: MaybeT ReadP (Int, Int)
-- getResolution = do
--   liftMaybeT skipSpaces
--   width' <- liftMaybeT $ munch isDigit
--   let width = read width'
--   liftMaybeT skipSpaces
--   liftMaybeT $ char 'x'
--   height' <- liftMaybeT $ munch isDigit
--   let height = read height'
--   pure (width, height)

prompt :: String -> MaybeT IO ()
prompt str = do 
  liftMaybeT $ putStr str
  liftMaybeT $ hFlush stdout

getParameters :: MaybeT IO Specs
getParameters = do
  prompt resolutionOptions
  (w, h) <- getResolution
  prompt "Minimum star radius (in pixels)? "
  minRad <- getRadius
  prompt "Maximum star radius (in pixels)? "
  maxRad <- getRadius
  prompt "Background color RGB (e.g. 100, 200, 50)? "
  bgCol <- getColor
  prompt "First star color RGB (e.g. 100, 200, 50)? "
  starCol1 <- getColor
  prompt "Second star color RGB (e.g. 100, 200, 50)? "
  starCol2 <- getColor
  pure $ Specs { width = w
               , height = h
               , starSizeRange = (minRad, maxRad)
               , starColors = (starCol1, starCol2)
               , bgColor = bgCol
               }

