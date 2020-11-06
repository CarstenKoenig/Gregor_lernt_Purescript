module Game 
    ( run
    ) where

import Prelude

import Effect (Effect)
import Graphics.Canvas (CanvasElement, getContext2D, withContext)
import Graphics.Canvas as Canvas
import Loop as Loop

run :: CanvasElement -> Effect Unit
run canvasElement = do
    let config = loopConfig canvasElement
    Loop.run config

data Event
  = Tick Loop.Delta
  | NoOp

type State =
    {

    }

initState :: Effect State
initState = pure { }

update :: Event -> State -> Effect State
update NoOp state = pure state
update (Tick delta) state = pure state

drawState :: CanvasElement -> State -> Effect Unit
drawState canvas _ = do
    Canvas.setCanvasWidth canvas 1000.0
    Canvas.setCanvasHeight canvas 1000.0
    ctx <- getContext2D canvas
    withContext ctx do
        Canvas.setFillStyle ctx "green"
        Canvas.fillRect ctx { x: 100.0, y: 100.0, width: 100.0, height: 100.0 }

-- ====================================================
-- Setup Game-Loop

loopConfig :: CanvasElement -> Loop.Config State Event
loopConfig canvasElement =
    { tickEvent: Tick
    , keyEvents: keys
    , getInitialState: initState
    , update
    , view: drawState canvasElement
    }
  where
  keys =
    [ { keyCode: 37, event: NoOp }
    , { keyCode: 39, event: NoOp }
    , { keyCode: 38, event: NoOp }
    , { keyCode: 40, event: NoOp }
    ]