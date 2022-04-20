{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module S where

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class
import Data.IORef
import Data.Word
import SDL
import SDL.Framerate

data Env = Env
  { renderer :: Renderer,
    frameManager :: Manager,
    lastTimeRef :: IORef Word32
  }

data Control = Stop

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  lastTimeRef <- newIORef @Word32 0
  frameManager <- manager
  set frameManager 3
  let env = Env renderer frameManager lastTimeRef
  runReader @Env env $
    runError @Control $ appLoop
  destroyWindow window

appLoop :: (Has (Reader Env :+: Error Control) sig m, MonadIO m) => m ()
appLoop = forever $ do
  Env {renderer, frameManager, lastTimeRef} <- ask @Env
  currentTime <- ticks
  lastTime <- liftIO $ readIORef lastTimeRef
  liftIO $ writeIORef lastTimeRef currentTime
  let diffTime = currentTime - lastTime
  liftIO $ print diffTime
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed
          -- && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          QuitEvent -> True
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  delay_ frameManager
  liftIO $ print events
  liftIO $ print (map eventIsQPress events)
  liftIO $ print qPressed
  when qPressed $ throwError Stop
