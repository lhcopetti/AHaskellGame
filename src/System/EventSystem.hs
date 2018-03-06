module System.EventSystem
    ( pollClosingEvent
    , pollAllEvents
    , shouldCloseWindow
    ) where

import SFML.Graphics.Types (RenderWindow)
import SFML.Graphics.RenderWindow (pollEvent)
import SFML.Window.Event (SFEvent (..))
import SFML.Window.Keyboard (KeyCode (KeyEscape))

import Control.Monad.IO.Class (liftIO)

import Control.Monad.Trans.Maybe (MaybeT (..))

pollAllEvents :: RenderWindow -> IO [SFEvent]
pollAllEvents window = go window []
    where
        go :: RenderWindow -> [SFEvent] -> IO [SFEvent]
        go wnd evts = do
            newEvt <- pollEvent wnd
            case newEvt of 
                Nothing -> return evts
                Just evt -> go wnd (evt : evts)

pollClosingEvent :: RenderWindow -> MaybeT IO SFEvent
pollClosingEvent window = do 
    evt <- pollEventT window

    case evt of
        (SFEvtKeyPressed code _ _ _ _) -> liftIO $ putStrLn ("Key pressed: " ++ show code)
        _ -> return ()

    if shouldCloseWindow evt then 
        return evt 
    else 
        pollClosingEvent window

pollEventT :: RenderWindow -> MaybeT IO SFEvent
pollEventT = MaybeT . pollEvent

shouldCloseWindow :: SFEvent -> Bool
shouldCloseWindow SFEvtClosed                           = True
shouldCloseWindow SFEvtMouseButtonPressed {}            = True
shouldCloseWindow (SFEvtKeyPressed KeyEscape _ _ _ _)   = True
shouldCloseWindow _                                     = False