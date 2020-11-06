module Game.Direction where

import Prelude

import Game.Coord (Coord)

data Direction
    = Up
    | Down
    | Left
    | Right

derive instance eqDirection :: Eq Direction

opposite :: Direction -> Direction
opposite Up = Down
opposite Down = Up
opposite Left = Right
opposite Right = Left

translate :: Direction -> Coord -> Coord
translate Up { x, y } = { x, y: y-1 }
translate Down { x, y } = { x, y: y+1 }
translate Left { x, y } = { x: x-1, y }
translate Right { x, y } = { x: x+1, y }