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
import Signal (constant, merge, mergeMany, runSignal) as Signal
import Signal.DOM (keyPressed) as Signal
import Signal.Effect (foldEffect) as Signal
import Signal.Time (every) as Signal

main :: Effect Unit
main = do
  log "Hey - Purescript is working"
  game <- Game.newApple $ Game.initialize { width: 50, height: 50 }
  canvasElement <- getCanvasElementById "game"
  stateSig <- stateSignal game
  for_ canvasElement $ drawSignal stateSig >>> Signal.runSignal

stateSignal :: Game.State -> Effect (Signal Game.State)
stateSignal init = do
  dirSignal <- directionSignal
  (tickSignal `Signal.merge` dirSignal) # Signal.foldEffect (\change state -> change state) init

tickSignal :: Signal (Game.State -> Effect Game.State)
tickSignal = Signal.every 500.0 $> Game.iter

directionSignal :: Effect (Signal (Game.State -> Effect Game.State))
directionSignal = do
  left <- keySignal 37 Game.Left
  right <- keySignal 39 Game.Right
  up <- keySignal 38 Game.Up
  down <- keySignal 40 Game.Down
  pure $ fromMaybe doNothingSignal $ Signal.mergeMany [ left, right, up, down ]
  where
  doNothingSignal :: Signal (Game.State -> Effect Game.State)
  doNothingSignal = Signal.constant (identity >>> pure)
  keySignal :: Int -> Game.Direction -> Effect (Signal (Game.State -> Effect Game.State))
  keySignal code dir = do
    signal <- Signal.keyPressed code 
    pure $ signal ~> onDown dir
  onDown :: Game.Direction -> Boolean -> (Game.State -> Effect Game.State)
  onDown dir true = Game.changeDirection dir >>> pure
  onDown _ false = identity >>> pure

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
drawState ctx { screen, snake, apple } = do
  for_ apple drawApple
  drawSnake
  where
  drawApple applePos =
    withContext ctx $ do
      Canvas.setStrokeStyle ctx "green"
      Canvas.setFillStyle ctx "lightgreen"
      drawBodyPart applePos
  drawSnake =
    withContext ctx $ do
      if Game.isDead snake then do
        Canvas.setStrokeStyle ctx "grey"
        Canvas.setFillStyle ctx "black"
      else do
        Canvas.setStrokeStyle ctx "pink"
        Canvas.setFillStyle ctx "red"
      for_ snake.tail drawBodyPart
      if Game.isDead snake then do
        Canvas.setStrokeStyle ctx "black"
        Canvas.setFillStyle ctx "grey"
      else do
        Canvas.setStrokeStyle ctx "red"
        Canvas.setFillStyle ctx "pink"
      drawBodyPart snake.head
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
    
