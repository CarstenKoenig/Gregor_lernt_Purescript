module Game.Coord where

import Prelude

import Data.Array (any)
import Data.Foldable (class Foldable)

type Coord = 
    { x :: Int
    , y :: Int
    }

isInside :: forall f. Foldable f => Coord -> f Coord -> Boolean
isInside pos poss =
    any (_ == pos) poss