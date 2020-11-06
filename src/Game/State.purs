module Game.State
    ( State
    , Time
    , initialize
    , iter
    , newApple
    , changeDirection
    ) where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Game.Coord (Coord, isInside)
import Game.Direction (Direction)
import Game.Screen (Screen)
import Game.Screen as Screen
import Game.Snake (Snake)
import Game.Snake as Snake

type Time = Number

type State =
    { screen :: Screen
    , snake :: Snake
    , apple :: Maybe Coord
    , stepEvery :: Time
    , timeToNextStep :: Time
    }

initialize :: Screen -> State
initialize screen =
    { screen
    , snake: Snake.initSnake screen
    , apple: Nothing
    , stepEvery: 500.0
    , timeToNextStep: 500.0
    }

iter :: Time -> State -> Effect State
iter delta state =
    go $ state { timeToNextStep = state.timeToNextStep - delta }
    where
    go :: State -> Effect State
    go s 
        | s.timeToNextStep <= 0.0 = do
            stepped <- step s
            go $ stepped { timeToNextStep = s.timeToNextStep + s.stepEvery }
        | otherwise = pure s

step :: State -> Effect State
step state | Snake.isDead state.snake = pure state
step state = do
    let eaten = tryEat state
    withApple <- newApple eaten
    let moved = withApple { snake = Snake.move withApple.screen withApple.snake }
    pure moved

tryEat :: State -> State
tryEat state@{ snake, apple } 
    | apple == Just snake.head =
        state { apple = Nothing
              , snake = Snake.grow snake 
              , stepEvery = state.stepEvery * 0.9
              }
tryEat state = state

newApple :: State -> Effect State
newApple state
    | isJust state.apple = 
        pure state
    | otherwise = do
        pos <- findApple
        pure $ state { apple = Just pos }
    where
      findApple = do
        pos <- Screen.randomCoord state.screen
        if not (pos `isInside` Snake.body state.snake) then
            pure pos
        else
            findApple

changeDirection :: Direction -> State -> State
changeDirection newDirection state =
    state { snake = Snake.changeDirection newDirection state.snake }