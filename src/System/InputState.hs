module System.InputState
    ( State (..)
    , Event (..)
    , emptyState
    , reduceEvents
    , stepState
    ) where

import SFML.Window.Event (SFEvent (..))
import SFML.Window.Mouse

data State = State  { justPressed   :: Bool
                    , isPressed     :: Bool
                    } deriving (Eq, Show)

data Event = Pressed | Released | Nil
    deriving (Show, Eq)

emptyState :: State
emptyState = State { justPressed = False, isPressed = False }

stepState :: State -> Event -> State
stepState _ Pressed = State { justPressed = True, isPressed = True }
stepState _ Released = emptyState
stepState state Nil = emptyState { isPressed = isPressed state }

reduceEvents :: [SFEvent] -> Event
reduceEvents = foldl go Nil
    where
        go _ (SFEvtMouseButtonPressed MouseLeft _ _)  = Pressed
        go _ (SFEvtMouseButtonReleased MouseLeft _ _) = Released
        go v _ = v