module Game 
    ( run
    ) where

import Prelude

import Effect (Effect)
import Game.Canvas as Canvas
import Game.Direction (Direction)
import Game.Direction as Direction
import Game.State (State)
import Game.State as State
import Graphics.Canvas (CanvasElement)
import Loop as Loop

run :: CanvasElement -> Effect Unit
run canvasElement = do
    let config = loopConfig canvasElement
    Loop.run config

data Event
  = Tick Loop.Delta
  | Input Direction

initState :: Effect State
initState =
  State.newApple $ State.initialize { width: 50, height: 50 }

update :: Event -> State -> Effect State
update (Tick delta) = State.iter delta
update (Input dir) = State.changeDirection dir >>> pure

loopConfig :: CanvasElement -> Loop.Config State Event
loopConfig canvasElement =
    { tickEvent: Tick
    , keyEvents: keys
    , getInitialState: initState
    , update
    , view: Canvas.drawState canvasElement
    }
  where
  keys =
    [ { keyCode: 37, event: Input Direction.Left }
    , { keyCode: 39, event: Input Direction.Right }
    , { keyCode: 38, event: Input Direction.Up }
    , { keyCode: 40, event: Input Direction.Down }
    ]