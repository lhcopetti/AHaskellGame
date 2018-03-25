module Physics.PhysicsCollision
    ( emptyCollisionData
    , collisionPoints
    , hasCollided
    ) where

import SFML.System.Vector2 (Vec2f)

import Physics.PhysicsTypes (PhyObject, PhyCollisionData)
import qualified Physics.Library.Hipmunk.HipmunkCollision as HMP
import Physics.Library.Hipmunk.VectorConversion (hVectorToVec2f)

emptyCollisionData :: PhyCollisionData
emptyCollisionData = HMP.emptyCollisionData

collisionPoints :: PhyCollisionData -> [Vec2f]
collisionPoints = map hVectorToVec2f . HMP.collisionPoints

hasCollided :: PhyObject -> PhyCollisionData -> Bool
hasCollided = HMP.hasCollided