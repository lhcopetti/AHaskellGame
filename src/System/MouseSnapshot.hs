module System.MouseSnapshot
    ( MouseSnapshot (..)
    , MouseEvt (..)
    , reduceLeftEvents
    , stepMouseSnapshot
    , stepSnapshot
    , emptySnapshot
    ) where

import SFML.Window.Event (SFEvent (..))
import SFML.Window.Mouse

data MouseSnapshot = MouseSnapshot  { leftJustPressed   :: Bool
                                    , leftIsPressed     :: Bool
                                    } deriving (Eq, Show)

emptySnapshot :: MouseSnapshot
emptySnapshot = MouseSnapshot { leftJustPressed = False, leftIsPressed = False }

data MouseEvt = Pressed | Released | Nil
    deriving (Show, Eq)

stepMouseSnapshot :: MouseSnapshot -> [SFEvent] -> MouseSnapshot
stepMouseSnapshot snap = stepSnapshot snap . reduceLeftEvents

stepSnapshot :: MouseSnapshot -> MouseEvt -> MouseSnapshot
stepSnapshot _ Pressed = MouseSnapshot { leftJustPressed = True, leftIsPressed = True }
stepSnapshot _ Released = emptySnapshot
stepSnapshot snap Nil = snap { leftJustPressed = False, leftIsPressed = leftIsPressed snap }

reduceLeftEvents :: [SFEvent] -> MouseEvt
reduceLeftEvents = foldl go Nil
    where
        go _ (SFEvtMouseButtonPressed MouseLeft _ _)  = Pressed
        go _ (SFEvtMouseButtonReleased MouseLeft _ _) = Released
        go v _ = v