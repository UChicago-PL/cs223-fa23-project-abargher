module UserInterface where

import Text.ParserCombinators.ReadP
import GHC.Data.Maybe
import Stars (Specs(..), Color)


-- type Color = (Double, Double, Double, Double)

-- data Specs = Specs { width :: Int
--                    , height :: Int
--                    , starSizeRange :: (Int, Int)
--                    , starColors :: (Color, Color)
--                    , bgColor :: Color
--                    }


getParameters :: MaybeT IO Specs
getParameters = do
  liftMaybeT $ putStrLn "hello"
  pure $ Specs {}