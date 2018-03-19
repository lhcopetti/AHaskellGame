module System.Input.MouseSnapshot
    ( MButton (..)
    , MouseSnapshot
    , mkMouseSnapshot
    , getButton
    , emptySnapshot
    , reduceEvents
    , stepMouseSnapshot
    ) where

import SFML.Window.Event (SFEvent (..))
import SFML.Window.Mouse

import qualified System.Input.InputState as I

data MouseSnapshot = MouseSnapshot  { left :: I.State
                                    , right :: I.State
                                    } deriving (Eq, Show)

mkMouseSnapshot :: I.State -> I.State -> MouseSnapshot
mkMouseSnapshot = MouseSnapshot

data MButton = MLeft | MRight
    deriving (Eq, Show)

getButton :: MButton -> MouseSnapshot -> I.State
getButton MLeft = left
getButton MRight = right

emptySnapshot :: MouseSnapshot
emptySnapshot = MouseSnapshot { left = I.emptyState, right = I.emptyState }

stepMouseSnapshot :: MouseSnapshot -> [SFEvent] -> MouseSnapshot
stepMouseSnapshot snap evts = MouseSnapshot 
    { left = update MouseLeft (left snap)
    , right = update MouseRight (right snap) 
    }
    where
        update bt st = I.stepState st (reduceEvents bt evts)

reduceEvents :: MouseButton -> [SFEvent] -> I.Event
reduceEvents mb = foldl go I.Nil
    where
        go v (SFEvtMouseButtonPressed  mb' _ _)  = if mb' == mb then I.Pressed else v
        go v (SFEvtMouseButtonReleased mb' _ _)  = if mb' == mb then I.Released else v 
        go v _ = v