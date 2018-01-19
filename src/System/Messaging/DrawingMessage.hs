module System.Messaging.DrawingMessage
    ( DrawingMessageType 
    , DrawingMessage (..)
    , DrawingInbox
    , getInbox
    , setInbox
    ) where

import Component.Draw.DrawingData (Drawing)

type DrawingMessageType = Drawing -> IO ()
data DrawingMessage = MSG DrawingMessageType

class DrawingInbox a where
    getInbox :: a -> [DrawingMessage]
    setInbox :: [DrawingMessage] -> a -> a