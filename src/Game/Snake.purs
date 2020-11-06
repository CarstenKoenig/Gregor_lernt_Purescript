module Game.Snake where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Game.Coord (Coord, isInside)
import Game.Direction (Direction)
import Game.Direction as Direction
import Game.Screen (Screen)
import Game.Screen as Screen

type Snake = 
    { head :: Coord
    , tail :: Array Coord
    , curDirection :: Direction
    , nextDirection :: Direction
    }

initSnake :: Screen -> Snake
initSnake screen =
    { head
    , tail: [ middle, arse ]
    , curDirection: Direction.Right
    , nextDirection: Direction.Right
    }
    where
    head =
        { x: screen.width `div` 2
        , y: screen.height `div` 2
        }
    middle = Direction.translate Direction.Left head
    arse = Direction.translate Direction.Left middle

isDead :: Snake -> Boolean
isDead snake =
    snake.head `isInside` snake.tail

body :: Snake -> Array Coord
body snake =
    snake.head `Array.cons` snake.tail

move :: Screen -> Snake -> Snake
move screen snake = 
    snake { head = newHead
          , tail = newTail
          , curDirection = snake.nextDirection }
    where
    newHead = Screen.warpCoord screen $ Direction.translate snake.nextDirection snake.head
    newTail = fromMaybe [] do
        { init } <- Array.unsnoc snake.tail
        pure $ Array.cons snake.head init

grow :: Snake -> Snake
grow snake =
    snake { tail = newTail }
    where
    newTail = fromMaybe [ snake.head ] $ do
        { last } <- Array.unsnoc snake.tail
        pure $ Array.snoc snake.tail last

changeDirection :: Direction -> Snake -> Snake
changeDirection newDirection snake
    | snake.curDirection /= Direction.opposite newDirection = 
        snake { nextDirection = newDirection }
    | otherwise = snake
