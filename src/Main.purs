module Main where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas (CanvasElement, fillRect, getCanvasElementById, getContext2D, setFillStyle, withContext)
import Graphics.Canvas as Canvas

main :: Effect Unit
main = do
  log "Hey - Purescript is working"
  canvasElement <- getCanvasElementById "game"
  for_ canvasElement drawSomething


drawSomething :: CanvasElement -> Effect Unit
drawSomething canvasEl = do
  -- set coordinate-system to 0-999 / 0-999
  Canvas.setCanvasWidth canvasEl 1000.0
  Canvas.setCanvasHeight canvasEl 1000.0
  ctx <- getContext2D canvasEl
  withContext ctx $ do
    setFillStyle ctx "blue"
    fillRect ctx myRect
  where
    myRect = 
      { x: 250.0
      , y: 250.0
      , height: 500.0
      , width: 500.0
      }
