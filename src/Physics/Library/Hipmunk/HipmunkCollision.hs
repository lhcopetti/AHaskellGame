module Physics.Library.Hipmunk.HipmunkCollision
    ( collisionPoints
    , emptyCollisionData
    ) where

import qualified Physics.Hipmunk as H

import Physics.Library.Hipmunk.PhysicsTypes

emptyCollisionData :: PhyCollisionData
emptyCollisionData = CD { points = [] 
                        }

collisionPoints :: PhyCollisionData -> [H.Position]
collisionPoints = points

