module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Player = Player {
  playerPosition :: Point
                     }

window :: Display
window = InWindow "haskell-game" (500, 500) (50, 50)

bg :: Color
bg = light black

drawing :: Player -> Picture
drawing p = pictures [ uncurry translate (playerPosition p) $ color green $ circleSolid 10  ]

handleKeyPress :: Event -> Player -> Player
handleKeyPress (EventKey (Char 'w') Down _ _) player = Player ((fst $ playerPosition player),      (snd $ playerPosition player) + 10)
handleKeyPress (EventKey (Char 'a') Down _ _) player = Player ((fst $ playerPosition player) - 10, (snd $ playerPosition player))
handleKeyPress (EventKey (Char 's') Down _ _) player = Player ((fst $ playerPosition player),      (snd $ playerPosition player) - 10)
handleKeyPress (EventKey (Char 'd') Down _ _) player = Player ((fst $ playerPosition player) + 10, (snd $ playerPosition player))
handleKeyPress _ player                           = player

timestep :: Float -> Player -> Player
timestep t player = player

main :: IO ()
main = do
  let player = Player (0, 0)
  
  play window bg 60 player drawing handleKeyPress timestep
