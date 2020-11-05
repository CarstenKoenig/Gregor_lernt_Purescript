module Main where

import Prelude

import Data.Foldable (for_)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Console (log)
import Game as Game
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getContext2D, withContext)
import Graphics.Canvas as Canvas

main :: Effect Unit
main = do
  log "Hey - Purescript is working"
  let game = Game.initialize { width: 50, height: 50 }
  canvasElement <- getCanvasElementById "game"
  for_ canvasElement (drawGame game)

drawGame :: Game.State -> CanvasElement -> Effect Unit
drawGame game canvasEl = do
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
      for_ snake drawBodyPart
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
    
