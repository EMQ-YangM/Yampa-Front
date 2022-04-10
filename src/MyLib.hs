{-# LANGUAGE Arrows #-}
{-# LANGUAGE NumericUnderscores #-}

module MyLib where

import Control.Concurrent
import Data.IORef
import FRP.Yampa as Yampa
import Graphics.UI.SDL as SDL

width :: Num a => a
width = 800

height :: Num a => a
height = 800

main = do
  timeRef <- newIORef (0 :: Int)
  controllerRef <- newIORef $ Controller (0, 0) False
  reactimate
    (initGraphs >> readIORef controllerRef)
    ( \_ -> do
        dtSecs <- yampaSDLTimeSense timeRef
        mInput <- sdlGetController controllerRef
        -- print (mInput)
        return (dtSecs, Just mInput)
    )
    (\_ e -> display (e))
    player

player :: SF Controller Controller
player = proc (Controller pos isq) -> do
  icpos <- inCircles -< pos
  returnA -< Controller icpos isq

-- | Coordinate of a body going in circles around another body.
inCircles :: SF (Double, Double) (Double, Double)
inCircles = proc (centerX, centerY) -> do
  t <- time -< ()
  let x = centerX + cos t * radius
      y = centerY + sin t * radius
      radius = 30
  returnA -< (x, y)

-- * SDL stuff

-- ** Input subsystem

-- | Input controller
data Controller = Controller
  { controllerPos :: (Double, Double),
    isQuit :: Bool
  }

-- | Give a controller, refresh its state and return the latest value.
-- We need a non-blocking controller-polling function.
sdlGetController :: IORef Controller -> IO Controller
sdlGetController controllerState = do
  state <- readIORef controllerState
  e <- pollEvent
  case e of
    MouseMotion x y _ _ ->
      writeIORef controllerState (Controller (fromIntegral x, fromIntegral y) False)
        >> sdlGetController controllerState
    Quit -> pure state {isQuit = True}
    KeyDown (Keysym SDLK_ESCAPE _ _) -> pure state {isQuit = True}
    _ -> return state

-- * Graphics

-- | Initialise rendering system.
initGraphs :: IO ()
initGraphs = do
  -- Initialise SDL
  SDL.init [InitVideo]

  -- Create window
  _ <- SDL.setVideoMode width height 16 [SWSurface]
  SDL.setCaption "Test" ""

display :: Controller -> IO Bool
display (Controller (playerX, playerY) isQ) = do
  if isQ
    then SDL.quit >> pure True
    else do
      -- Obtain surface
      screen <- getVideoSurface

      -- Paint screen green
      let format = surfaceGetPixelFormat screen
      bgColor <- mapRGB format 55 60 64
      fillRect screen Nothing bgColor

      -- Paint small red square, at an angle 'angle' with respect to the center
      foreC <- mapRGB format 212 108 73
      let side = 40
          x = round playerX
          y = round playerY
      fillRect screen (Just (Rect (x - 20) (y - 20) side side)) foreC
      -- fillRect screen (Just (Rect x y side side)) foreC

      -- Double buffering
      SDL.flip screen
      threadDelay 16_000
      pure False

yampaSDLTimeSense :: IORef Int -> IO Yampa.DTime
yampaSDLTimeSense timeRef = do
  -- Get time passed since SDL init
  newTime <- fmap fromIntegral SDL.getTicks

  -- Obtain time difference
  dt <- updateTime timeRef newTime
  let dtSecs = fromIntegral dt / 100
  return dtSecs

updateTime :: IORef Int -> Int -> IO Int
updateTime timeRef newTime = do
  previousTime <- readIORef timeRef
  writeIORef timeRef newTime
  return (newTime - previousTime)
