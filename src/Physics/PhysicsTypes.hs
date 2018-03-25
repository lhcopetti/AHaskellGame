module Physics.PhysicsTypes
    ( PhysicsWorld
    , PhyObject
    , PhyCollisionData
    ) where

import qualified Physics.Library.Hipmunk.PhysicsTypes as HMP

type PhysicsWorld   = HMP.PhysicsWorld
type PhyObject = HMP.PhysicsObject

type PhyCollisionData = HMP.PhyCollisionData