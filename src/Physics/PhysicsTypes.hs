module Physics.PhysicsTypes
    ( PhysicsWorld
    , PhysicsLibrary
    ) where

import qualified Physics.Library.Hipmunk.PhysicsTypes as HMP

type PhysicsWorld   = HMP.PhysicsWorld
type PhysicsLibrary = HMP.PhysicsObject