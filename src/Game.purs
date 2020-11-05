module Game where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)

type Coord = 
    { x :: Int
    , y :: Int
    }

type Screen =
    { width :: Int
    , height :: Int
    }

type Snake = 
    { head :: Coord
    , tail :: Array Coord
    , direction :: Direction
    }

type State =
    { screen :: Screen
    , snake :: Snake
    }

initialize :: Screen -> State
initialize screen =
    { screen
    , snake: 
        { head: mid
        , tail: [ translate Left mid, translate Left (translate Left mid) ]
        , direction: Right
        }
    }
    where
    mid =
        { x: screen.width `div` 2
        , y: screen.height `div` 2
        }

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

iter :: State -> State
iter state =
    state { snake = move state.snake }

move :: Snake -> Snake
move snake = 
    snake { head = newHead, tail = newTail }
    where
    newHead = translate snake.direction snake.head
    newTail = fromMaybe [] do
        { init } <- Array.unsnoc snake.tail
        pure $ Array.cons snake.head init

translate :: Direction -> Coord -> Coord
translate Up { x, y } = { x, y: y-1 }
translate Down { x, y } = { x, y: y+1 }
translate Left { x, y } = { x: x-1, y }
translate Right { x, y } = { x: x+1, y }

changeDirection :: Direction -> State -> State
changeDirection newDirection state
    | state.snake.direction /= opposite newDirection = 
        state { snake = state.snake { direction = newDirection } }
    | otherwise = state
