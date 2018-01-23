module System.EventSystem
    ( pollClosingEvent
    ) where

import SFML.Graphics.Types (RenderWindow)
import SFML.Graphics.RenderWindow (pollEvent)
import SFML.Window.Event (SFEvent (..))

import Control.Monad.Trans.Maybe (MaybeT (..))

pollClosingEvent :: RenderWindow -> MaybeT IO SFEvent
pollClosingEvent window = do 
    evt <- pollEventT window
    if shouldCloseWindow evt then 
        return evt 
    else 
        pollClosingEvent window

pollEventT :: RenderWindow -> MaybeT IO SFEvent
pollEventT = MaybeT . pollEvent

shouldCloseWindow :: SFEvent -> Bool
shouldCloseWindow SFEvtClosed                   = True
shouldCloseWindow SFEvtMouseButtonPressed {}    = True
shouldCloseWindow _                             = False