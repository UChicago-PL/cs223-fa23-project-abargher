module UserInterface where

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

getResolution :: MaybeT IO (Int, Int)
getResolution = do
  line <- liftMaybeT getLine
  let line' = splitOn "x" $ filter (not . isSpace) line
  guard (length line' == 2)
  let width = read $ head line'
  let height = read $ last line'
  pure (width, height)

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
prompt str = liftMaybeT $ putStrLn str

getParameters :: MaybeT IO (Int, Int)
getParameters = do
  prompt "Image resolution (e.g. 1920 x 1080)? "
  (w, h) <- getResolution
  prompt "Image resolution 2? "
  (w2, h2) <- getResolution
  pure (w, h)
