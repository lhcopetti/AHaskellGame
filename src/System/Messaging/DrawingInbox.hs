module System.Messaging.DrawingInbox
    ( DrawingInbox
    , getInbox
    , addInbox
    , clearInbox
    ) where

import GameObject.GameObjectTypes (DrawingMessage)

class DrawingInbox a where
    getInbox :: a -> [DrawingMessage]
    addInbox :: DrawingMessage -> a -> a
    clearInbox :: a -> a