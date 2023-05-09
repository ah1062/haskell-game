module Main where

import Data.Char
import qualified Data.Set as S

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Player = Player {
  playerActions  :: S.Set Char,
  playerPosition :: Point
                     }

translateP :: Point -> Point -> Point
translateP (x,y) (x2,y2) = (x+x2, y+y2)

window :: Display
window = InWindow "haskell-game" (500, 500) (50, 50)

bg :: Color
bg = light black

drawing :: Player -> Picture
drawing p = pictures [ uncurry translate (playerPosition p) $ color green $ circleSolid 10  ]

handleKeyPress :: Event -> Player -> Player
handleKeyPress (EventKey (Char c) Down _ _) player
  | toUpper c == 'W'    = Player (S.insert c (playerActions player)) (translateP (playerPosition player) (0, 10))
  | toUpper c == 'A'    = Player (S.insert c (playerActions player)) (translateP (playerPosition player) (-10, 0))
  | toUpper c == 'S'    = Player (S.insert c (playerActions player)) (translateP (playerPosition player) (0, -10))
  | toUpper c == 'D'    = Player (S.insert c (playerActions player)) (translateP (playerPosition player) (10, 0))
  | otherwise           = player
handleKeyPress (EventKey (Char c) Up _ _) player = Player (S.delete c (playerActions player)) (playerPosition player)
handleKeyPress _ player = player

timestep :: Float -> Player -> Player
timestep t player = do
  let acts = S.toList (playerActions player)
  handleActions player acts
    where handleActions :: Player -> [Char] -> Player
	  handleActions p []     = p
	  handleActions p (c:cs) = handleActions (handleKeyPress (EventKey (Char c) Down (Modifiers Up Up Up) (0,0)) p) cs 

main :: IO ()
main = do
  let player = Player S.empty (0, 0)
  
  play window bg 60 player drawing handleKeyPress timestep
