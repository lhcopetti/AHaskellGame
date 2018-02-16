module Physics.PhysicsTypes
    ( PhysicsWorld
    , PhyObject
    ) where

import qualified Physics.Library.Hipmunk.PhysicsTypes as HMP

type PhysicsWorld   = HMP.PhysicsWorld
type PhyObject = HMP.PhysicsObject