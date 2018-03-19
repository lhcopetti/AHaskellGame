module System.Messaging.PhysicsInbox
    ( PhysicsInbox
    , getInbox
    , addInbox
    , clearInbox
    ) where

import GameObject.GameObjectTypes (PhysicsMessage)

class PhysicsInbox a where
    getInbox :: a -> [PhysicsMessage]
    addInbox :: PhysicsMessage -> a -> a
    clearInbox :: a -> a