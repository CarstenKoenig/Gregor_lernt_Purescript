module Loop 
    ( Config
    , Delta
    , KeyCode
    , run
    ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Effect (Effect)
import Signal (Signal, (~>))
import Signal as Signal
import Signal.DOM as SignalDOM
import Signal.Effect as SignalEffect

type Delta = Number
type KeyCode = Int

type Config state event =
    { tickEvent :: Delta -> event
    , keyEvents :: Array ({ keyCode :: KeyCode, event :: event })
    , getInitialState :: Effect state
    , update :: event -> state -> Effect state
    , view :: state -> Effect Unit
    }

run :: forall state event. Config state event -> Effect Unit
run config = do
  eventSig <- configureEventSignal config
  initialState <- config.getInitialState
  stateSig <- eventSig # SignalEffect.foldEffect config.update initialState 
  let drawSig = stateSig ~> config.view
  Signal.runSignal drawSig

configureEventSignal :: forall state event. Config state event -> Effect (Signal event)
configureEventSignal config = do
  ticks <- configureTickSignal config
  keys <- configureKeysSignal config
  pure $ Signal.merge ticks keys

configureTickSignal :: forall state event. Config state event -> Effect (Signal event)
configureTickSignal config = do
  animFrameSig <- SignalDOM.animationFrame
  let 
    deltaSig =
      animFrameSig
        # Signal.foldp calcDelta Nothing
        # Signal.filterMap (map _.delta) 0.0 
  pure $ deltaSig ~> config.tickEvent
  where
  calcDelta curTime Nothing = Just { lastTime: curTime, delta: 0.0 }
  calcDelta curTime (Just { lastTime }) = Just { lastTime: curTime, delta: curTime - lastTime }

configureKeysSignal :: forall state event. Config state event -> Effect (Signal event)
configureKeysSignal config = do
  signals <- for config.keyEvents keySignal
  pure $ fromMaybe doNothingSignal $ Signal.mergeMany signals
  where
  keySignal :: { keyCode :: KeyCode, event :: event } -> Effect (Signal event)
  keySignal { keyCode, event } = do
    signal <- SignalDOM.keyPressed keyCode 
    pure $ signal ~> onDown event
  onDown ev true = ev
  onDown _ false = nothingEvent
  -- use 0-delta tick as a *hack* for nothing
  nothingEvent = config.tickEvent 0.0
  doNothingSignal = Signal.constant nothingEvent
