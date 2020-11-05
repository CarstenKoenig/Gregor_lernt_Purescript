module Game where

import Prelude

type Coord = 
    { x :: Int
    , y :: Int
    }

type Screen =
    { width :: Int
    , height :: Int
    }

type Snake = 
    Array Coord

type State =
    { screen :: Screen
    , snake :: Snake
    }

initialize :: Screen -> State
initialize screen =
    { screen
    , snake: [ { x: screen.width `div` 2
               , y: screen.height `div` 2
               } 
             ]
    }