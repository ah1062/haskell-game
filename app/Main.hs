module Main where

import Data.Char (toUpper)
import Data.Maybe

import Graphics.Gloss
import Graphics.Gloss.Geometry.Line
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

pairToList :: (a, a) -> [a]
pairToList (x,y) = [x, y]
 
getBoundsSqCorners :: Vector -> [(Point, Point)]
getBoundsSqCorners (x,y) = [((-x,y), (x,y)), ((x,y), (x,-y)), ((x,-y), (-x,-y)), ((-x,-y), (-x,y))]

window :: Display
window = InWindow "haskell-game" (600, 600) (50, 50)

bg :: Color
bg = light black

fps :: Int
fps = 30

initialState :: World 
initialState = World (Player red (0, 0) Nothing) (250, 250)

drawing :: World -> IO Picture
drawing (World p (wbx,wby)) = return $ pictures [ 
                                 uncurry translate (playerPosition p) $ color (playerColor p) $ circleSolid 10,
                                 color (dark white) $ pictures $ map (line . pairToList) (getBoundsSqCorners (wbx, wby)),
                                 pictures [color (dark white) $ line [x, closestPointOnLine p1 p2 x] | let x = playerPosition p, i <- getBoundsSqCorners (wbx,wby), let (p1, p2) = i ]                                                           
                                                ]

handleEvent :: Event -> World -> IO World
handleEvent (EventKey (Char c) Down _ _) (World p wb)
  | toUpper c == 'W' = return $ World (Player (playerColor p) (translateP (0, 10)  (playerPosition p)) (Just (Char c))) wb
  | toUpper c == 'S' = return $ World (Player (playerColor p) (translateP (0, -10) (playerPosition p)) (Just (Char c))) wb
  | toUpper c == 'D' = return $ World (Player (playerColor p) (translateP (10, 0)  (playerPosition p)) (Just (Char c))) wb
  | toUpper c == 'A' = return $ World (Player (playerColor p) (translateP (-10, 0) (playerPosition p)) (Just (Char c))) wb
handleEvent (EventKey (Char c) Up _ _) (World p wb) = return $ World (Player (playerColor p) (playerPosition p) Nothing) wb
handleEvent _ p = return p

timestep :: Float -> World -> IO World
timestep _ (World p wb) = 
  case playerKey p of
    Just key -> do
      handleEvent (EventKey key Down (Modifiers Up Up Up) (0,0)) (World p wb)
    Nothing -> return $ World p wb

main :: IO ()
main = playIO window bg fps initialState drawing handleEvent timestep
