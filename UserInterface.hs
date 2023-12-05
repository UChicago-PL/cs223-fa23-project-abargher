module UserInterface where

import System.IO
import GHC.Data.Maybe
import Data.Char
import Data.List.Split
import Control.Monad
import InterfaceUtils

type Color = (Double, Double, Double, Double)

data Specs = Specs { width :: Int
                   , height :: Int
                   , starSizeRange :: (Int, Int)
                   , starColors :: (Color, Color)
                   , bgColor :: Color
                   , fileName :: FilePath
                   }
  deriving Show


getResolution :: MaybeT IO (Int, Int)
getResolution = do
  line' <- liftMaybeT getLine
  let line = filter (not . isSpace) line'
  if null line then
    pure $ resolutionMap !! 3
  else do 
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
  guard (length line' == 3 && all (all isDigit) line')
  let cols = map ((/ 255) . read) line'
  guard (all (\n -> n >= 0 && n <= 255) cols)
  let [r, g, b] = cols
  pure (r, g, b, 1.0)

getFname :: MaybeT IO FilePath
getFname = do
  line <- liftMaybeT getLine
  guard (not $ null line)
  pure line

prompt :: String -> MaybeT IO ()
prompt str = liftMaybeT $ do {putStr str; hFlush stdout} 

getParameters :: MaybeT IO Specs
getParameters = do
  prompt "enter destination filename to write image: "
  fname <- getFname
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
               , fileName = fname
               }

