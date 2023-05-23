module Main where

import Data.Char (toUpper)
import Data.Maybe

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data World = World {
  worldPlayer :: Player,
  worldBounds :: Vector
}

data Player = Player {
  playerColor    :: Color,
  playerPosition :: Point,
  playerKey      :: Maybe Key
}

translateP :: Point -> Point -> Point
translateP (x,y) (x2,y2) = (x+x2, y+y2)

window :: Display
window = InWindow "haskell-game" (600, 600) (50, 50)

bg :: Color
bg = light black

fps :: Int
fps = 30

initialState :: Player 
initialState = Player red (0, 0) Nothing

drawing :: Player -> IO Picture
drawing p = return $ pictures [ 
                                 uncurry translate (playerPosition p) $ color (playerColor p) $ circleSolid 10,
                                 color (dark white) $ pictures $ map line [ 
                                    [(-250, 250), (250, 250)], [(250, 250), (250, -250)], [(250, -250), (-250, -250)], [(-250,-250), (-250, 250)] 
                                                              ]
                              ]

handleEvent :: Event -> Player -> IO Player
handleEvent (EventKey (Char c) Down _ _) p
  | toUpper c == 'W' = return $ Player (playerColor p) (translateP (0, 10)  (playerPosition p)) (Just (Char c))
  | toUpper c == 'S' = return $ Player (playerColor p) (translateP (0, -10) (playerPosition p)) (Just (Char c))
  | toUpper c == 'D' = return $ Player (playerColor p) (translateP (10, 0)  (playerPosition p)) (Just (Char c))
  | toUpper c == 'A' = return $ Player (playerColor p) (translateP (-10, 0) (playerPosition p)) (Just (Char c))
handleEvent (EventKey (Char c) Up _ _) p = return $ Player (playerColor p) (playerPosition p) Nothing
handleEvent _ p = return p

timestep :: Float -> Player -> IO Player
timestep _ p = 
  case playerKey p of
    Just key -> do
      handleEvent (EventKey key Down (Modifiers Up Up Up) (0,0)) p
    Nothing -> return p

main :: IO ()
main = playIO window bg fps initialState drawing handleEvent timestep
