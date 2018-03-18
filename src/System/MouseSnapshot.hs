module System.MouseSnapshot
    ( MButton (..)
    , MouseSnapshot (..)
    , MouseEvt (..)
    , emptySnapshot
    , reduceEvents
    , stepMouseSnapshot
    ) where

import SFML.Window.Event (SFEvent (..))
import SFML.Window.Mouse

import qualified System.InputState as I

data MouseSnapshot = MouseSnapshot  { left :: I.State
                                    } deriving (Eq, Show)

data MButton = Left | Right
    deriving (Eq, Show)

emptySnapshot :: MouseSnapshot
emptySnapshot = MouseSnapshot { left = I.emptyState }

data MouseEvt = Pressed | Released | Nil
    deriving (Show, Eq)

stepMouseSnapshot :: MouseSnapshot -> [SFEvent] -> MouseSnapshot
stepMouseSnapshot snap evts = MouseSnapshot { left = update (left snap) }
    where
        update st = I.stepState st . I.reduceEvents $ evts

reduceEvents :: [SFEvent] -> MouseEvt
reduceEvents = foldl go Nil
    where
        go _ (SFEvtMouseButtonPressed MouseLeft _ _)  = Pressed
        go _ (SFEvtMouseButtonReleased MouseLeft _ _) = Released
        go v _ = v