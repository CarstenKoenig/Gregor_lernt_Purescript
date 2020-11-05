module Game where

import Prelude

import Data.Array (any)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect (Effect)
import Effect.Random (randomInt)

type Coord = 
    { x :: Int
    , y :: Int
    }

isInside :: forall f. Foldable f => Coord -> f Coord -> Boolean
isInside pos poss =
    any (_ == pos) poss

randomCoord :: Screen -> Effect Coord
randomCoord screen = do
    x <- randomInt 0 screen.width
    y <- randomInt 0 screen.height
    pure { x, y }

type Screen =
    { width :: Int
    , height :: Int
    }

warp :: Screen -> Coord -> Coord
warp { width, height } { x, y } =
    { x: x `mod` width 
    , y: y `mod` height
    }

type Snake = 
    { head :: Coord
    , tail :: Array Coord
    , curDirection :: Direction
    , nextDirection :: Direction
    }

body :: Snake -> Array Coord
body snake =
    snake.head `Array.cons` snake.tail

isDead :: Snake -> Boolean
isDead snake =
    snake.head `isInside` snake.tail

type State =
    { screen :: Screen
    , snake :: Snake
    , apple :: Maybe Coord
    }

initialize :: Screen -> State
initialize screen =
    { screen
    , snake: 
        { head
        , tail: [ middle, arse ]
        , curDirection: Right
        , nextDirection: Right
        }
    , apple: Nothing
    }
    where
    head =
        { x: screen.width `div` 2
        , y: screen.height `div` 2
        }
    middle = translate Left head
    arse = translate Left middle

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

iter :: State -> Effect State
iter state | isDead state.snake = pure state
iter state = do
    let eaten = tryEat state
    withApple <- newApple eaten
    let moved = withApple { snake = move withApple.screen withApple.snake }
    pure moved

move :: Screen -> Snake -> Snake
move screen snake = 
    snake { head = newHead
          , tail = newTail
          , curDirection = snake.nextDirection }
    where
    newHead = warp screen $ translate snake.nextDirection snake.head
    newTail = fromMaybe [] do
        { init } <- Array.unsnoc snake.tail
        pure $ Array.cons snake.head init

tryEat :: State -> State
tryEat state@{ snake, apple } 
    | apple == Just snake.head =
        state { apple = Nothing, snake = grow snake }
tryEat state = state

grow :: Snake -> Snake
grow snake =
    snake { tail = newTail }
    where
    newTail = fromMaybe [ snake.head ] $ do
        { last } <- Array.unsnoc snake.tail
        pure $ Array.snoc snake.tail last

newApple :: State -> Effect State
newApple state
    | isJust state.apple = 
        pure state
    | otherwise = do
        pos <- findApple
        pure $ state { apple = Just pos }
    where
      findApple = do
        pos <- randomCoord state.screen
        if not (pos `isInside` body state.snake) then
            pure pos
        else
            findApple

translate :: Direction -> Coord -> Coord
translate Up { x, y } = { x, y: y-1 }
translate Down { x, y } = { x, y: y+1 }
translate Left { x, y } = { x: x-1, y }
translate Right { x, y } = { x: x+1, y }

changeDirection :: Direction -> State -> State
changeDirection newDirection state
    | state.snake.curDirection /= opposite newDirection = 
        state { snake = state.snake { nextDirection = newDirection } }
    | otherwise = state
