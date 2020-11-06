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
    
