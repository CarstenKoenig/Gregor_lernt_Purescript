module Main where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Game as Game
import Graphics.Canvas (getCanvasElementById)

main :: Effect Unit
main = do
  canvasElement <- getCanvasElementById "game"
  for_ canvasElement Game.run