{-# LANGUAGE Arrows #-}

module L where

import Control.Arrow
import FRP.Yampa

-- data EventSource a =

stepper :: a -> SF (Maybe a) a
stepper a = proc (val) -> do
  case val of
    Nothing -> returnA -< a
    Just v -> returnA -< v

newtype Point = Point (Int, Int) deriving (Show)

data Mouse = Mouse
  { mpos :: Point,
    lbDown :: Bool,
    rbDown :: Bool
  }
  deriving (Show)

data Kbd = Kbd {keyDwon :: [Char]}

type GUIInput = (Maybe Kbd, Maybe Mouse)

data Picture = Picture

type GUI a b = SF (GUIInput, a) (Picture, b)

mouseSF :: SF GUIInput Point
mouseSF = proc (_, mouse) -> do
  mouse1 <- stepper undefined -< mouse
  returnA -< mpos mouse1

move :: Picture -> Point -> Picture
move = undefined

ballPic :: Picture
ballPic = undefined

ballGUI :: GUI () ()
ballGUI = proc (gi, _) -> do
  r <- mouseSF -< gi
  returnA -< (move ballPic r, ())

runGUI :: GUI a b -> IO ()
runGUI = undefined

data LabelConf = LabelConf

flabel :: GUI LabelConf ()
flabel = undefined

ltext :: String -> LabelConf
ltext = undefined

data ButtonConf = ButtonConf

btext :: String -> ButtonConf
btext = undefined

enabled :: Bool -> ButtonConf
enabled = undefined

fbutton :: GUI ButtonConf (Maybe ())
fbutton = undefined

butTest :: GUI () (Maybe ())
butTest = proc (inpS, _) -> do
  fbutton -< (inpS, btext "press me!")

aboveGUI :: GUI b c -> GUI d e -> GUI (b, d) (c, e)
aboveGUI = undefined

besideGUI :: GUI b c -> GUI d e -> GUI (b, d) (c, e)
besideGUI = undefined

hello :: GUI () (Maybe (), ())
hello = proc (inpS, _) -> do
  (fbutton `besideGUI` flabel) -< (inpS, (btext "press me", ltext " PLEASE! "))

stepAccum :: a -> SF (Maybe (a -> a)) a
stepAccum = undefined

ebind :: a -> Maybe b -> Maybe a
ebind = fmap . const
