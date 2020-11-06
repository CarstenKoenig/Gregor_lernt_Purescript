module Main where

import Prelude

import Data.Foldable (for_)
import Data.Int (toNumber)
import Effect (Effect)
import Game as Game
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getContext2D, withContext)
import Graphics.Canvas as Canvas
import Loop as Loop

main :: Effect Unit
main = do
  canvasElement <- getCanvasElementById "game"
  for_ canvasElement $ \el -> do
    let config = loopConfig el
    Loop.run config

data Event
  = Tick Loop.Delta
  | Input Game.Direction

initGame :: Effect Game.State
initGame =
  Game.newApple $ Game.initialize { width: 50, height: 50 }

update :: Event -> Game.State -> Effect Game.State
update (Tick delta) = Game.iter delta
update (Input dir) = Game.changeDirection dir >>> pure

loopConfig :: CanvasElement -> Loop.Config Game.State Event
loopConfig canvasElement =
    { tickEvent: Tick
    , keyEvents: keys
    , getInitialState: initGame
    , update
    , view: drawGame canvasElement
    }
  where
  keys =
    [ { keyCode: 37, event: Input Game.Left }
    , { keyCode: 39, event: Input Game.Right }
    , { keyCode: 38, event: Input Game.Up }
    , { keyCode: 40, event: Input Game.Down }
    ]

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
    drawRectTo "green" "lightgreen" applePos
  drawSnake = do
    for_ snake.tail (drawRectTo strokeTail fillTail)
    drawRectTo strokeHead fillHead snake.head
    where
    strokeHead | Game.isDead snake = "black"
    strokeHead | otherwise = "red"
    fillHead | Game.isDead snake = "grey"
    fillHead | otherwise = "pink"
    strokeTail = fillHead
    fillTail = strokeHead
  drawRectTo colorStroke colorFill pos = do
    let 
      { x, y } = toCanvasPoint pos
      rect = { x, y, width: dX, height: dY }
    withContext ctx do
      Canvas.setStrokeStyle ctx colorStroke
      Canvas.setFillStyle ctx colorFill
      Canvas.fillRect ctx rect
      Canvas.strokeRect ctx rect
  toCanvasPoint { x, y} =
    { x: toNumber x * dX
    , y: toNumber y * dY
    }
  dX = 1000.0 / toNumber screen.width
  dY = 1000.0 / toNumber screen.height
    
