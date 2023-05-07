module Main where

import Graphics.Gloss

window :: Display
window = InWindow "haskell-game" (500, 500) (50, 50)

bg :: Color
bg = light black

drawing :: Picture
drawing = pictures []


main :: IO ()
main = do
  display window bg drawing
