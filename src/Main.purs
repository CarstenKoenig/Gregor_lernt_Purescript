module Main where

import Prelude

import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Game as Game
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getContext2D, withContext)
import Graphics.Canvas as Canvas
import Signal (Signal, (~>))
import Signal (constant, foldp, merge, mergeMany, runSignal) as Signal
import Signal.DOM (keyPressed) as Signal
import Signal.Time (every) as Signal

main :: Effect Unit
main = do
  log "Hey - Purescript is working"
  let game = Game.initialize { width: 50, height: 50 }
  canvasElement <- getCanvasElementById "game"
  stateSig <- stateSignal game
  for_ canvasElement $ drawSignal stateSig >>> Signal.runSignal

stateSignal :: Game.State -> Effect (Signal Game.State)
stateSignal init = do
  dirSignal <- directionSignal
  pure $
    (tickSignal `Signal.merge` dirSignal)
    # Signal.foldp (\change state -> change state) init

tickSignal :: Signal (Game.State -> Game.State)
tickSignal = Signal.every 500.0 $> Game.iter

directionSignal :: Effect (Signal (Game.State -> Game.State))
directionSignal = do
  left <- keySignal 37 Game.Left
  right <- keySignal 39 Game.Right
  up <- keySignal 38 Game.Up
  down <- keySignal 40 Game.Down
  pure $ fromMaybe doNothingSignal $ Signal.mergeMany [ left, right, up, down ]
  where
  doNothingSignal :: Signal (Game.State -> Game.State)
  doNothingSignal = Signal.constant identity
  keySignal :: Int -> Game.Direction -> Effect (Signal (Game.State -> Game.State))
  keySignal code dir = do
    signal <- Signal.keyPressed code 
    pure $ signal ~> onDown dir
  onDown :: Game.Direction -> Boolean -> (Game.State -> Game.State)
  onDown dir true = Game.changeDirection dir
  onDown _ false = identity

drawSignal :: Signal Game.State -> CanvasElement -> Signal (Effect Unit)
drawSignal stateSig canvasElement =
  stateSig ~> drawGame canvasElement 

drawGame :: CanvasElement -> Game.State -> Effect Unit
drawGame canvasEl game = do
  -- set coordinate-system to 0-999 / 0-999
  Canvas.setCanvasWidth canvasEl 1000.0
  Canvas.setCanvasHeight canvasEl 1000.0
  ctx <- getContext2D canvasEl
  drawState ctx game

drawState :: Context2D -> Game.State -> Effect Unit
drawState ctx { screen, snake } =
    drawSnake
  where
  drawSnake =
    withContext ctx $ do
      Canvas.setStrokeStyle ctx "red"
      Canvas.setFillStyle ctx "pink"
      for_ (Game.body snake) drawBodyPart
  drawBodyPart bodyPos = do
    let 
      { x, y } = toCanvasPoint bodyPos
      bodyRectangle =
        { x, y
        , width: dX
        , height: dY
        }
    Canvas.fillRect ctx bodyRectangle
    Canvas.strokeRect ctx bodyRectangle
  toCanvasPoint { x, y} =
    { x: toNumber x * dX
    , y: toNumber y * dY
    }
  dX = 1000.0 / toNumber screen.width
  dY = 1000.0 / toNumber screen.height
    
