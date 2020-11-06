module Game.Canvas 
    ( drawState
    ) where

import Prelude

import Data.Foldable (for_)
import Data.Int (toNumber)
import Effect (Effect)
import Game.Snake as Snake
import Game.State (State)
import Graphics.Canvas (CanvasElement, Context2D, getContext2D, withContext)
import Graphics.Canvas as Canvas

drawState :: CanvasElement -> State -> Effect Unit
drawState canvasEl game = do
  -- set coordinate-system to 0-999 / 0-999
  Canvas.setCanvasWidth canvasEl canvasWidth
  Canvas.setCanvasHeight canvasEl canvasHeight
  ctx <- getContext2D canvasEl
  drawToContext ctx game

canvasWidth :: Number
canvasWidth = 1000.0

canvasHeight :: Number
canvasHeight = 1000.0

drawToContext :: Context2D -> State -> Effect Unit
drawToContext ctx { screen, snake, apple } = do
  for_ apple drawApple
  drawSnake
  where
  drawApple applePos =
    drawRectTo "green" "lightgreen" applePos
  drawSnake = do
    for_ snake.tail (drawRectTo strokeTail fillTail)
    drawRectTo strokeHead fillHead snake.head
    where
    strokeHead | Snake.isDead snake = "black"
    strokeHead | otherwise = "red"
    fillHead | Snake.isDead snake = "grey"
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
  dX = canvasWidth / toNumber screen.width
  dY = canvasHeight / toNumber screen.height
    
