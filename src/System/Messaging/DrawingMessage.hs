module System.Messaging.DrawingMessage
    ( DrawingMessageType 
    , DrawingMessage (..)
    , DrawingInbox
    , getInbox
    , setInbox
    , clearInbox
    ) where

import Component.Draw.DrawingData (Drawing)

type DrawingMessageType = Drawing -> IO ()
data DrawingMessage = MSG DrawingMessageType
                    | NamedMessage String DrawingMessageType

class DrawingInbox a where
    getInbox :: a -> [DrawingMessage]
    setInbox :: [DrawingMessage] -> a -> a
    clearInbox :: a -> a