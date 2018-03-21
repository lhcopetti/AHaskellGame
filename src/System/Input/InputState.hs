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
stepState s Pressed = 
    let isPressed' = True 
        justPressed' = not . isPressed $ s
    in State { isPressed = isPressed', justPressed = justPressed' }
stepState _ Released = emptyState
stepState state Nil = emptyState { isPressed = isPressed state }