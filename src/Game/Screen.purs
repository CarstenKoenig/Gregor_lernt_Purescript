module Game.Screen where

import Prelude

import Effect (Effect)
import Effect.Random (randomInt)
import Game.Coord (Coord)

type Screen =
    { width :: Int
    , height :: Int
    }

randomCoord :: Screen -> Effect Coord
randomCoord screen = do
    x <- randomInt 0 (screen.width - 1)
    y <- randomInt 0 (screen.height - 1)
    pure { x, y }

warpCoord :: Screen -> Coord -> Coord
warpCoord { width, height } { x, y } =
    { x: x `mod` width 
    , y: y `mod` height
    }