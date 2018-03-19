module System.Input.InputState
    ( State (..)
    , Event (..)
    , emptyState
    , stepState
    ) where

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