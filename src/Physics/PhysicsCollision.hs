module Physics.PhysicsCollision
    ( emptyCollisionData
    , collisionPoints
    ) where

import SFML.System.Vector2 (Vec2f)

import qualified Physics.Library.Hipmunk.HipmunkCollision as HMP
import Physics.Library.Hipmunk.PhysicsTypes (PhyCollisionData (..))
import Physics.Library.Hipmunk.VectorConversion (hVectorToVec2f)

emptyCollisionData :: PhyCollisionData
emptyCollisionData = HMP.emptyCollisionData

collisionPoints :: PhyCollisionData -> [Vec2f]
collisionPoints = map hVectorToVec2f . HMP.collisionPoints