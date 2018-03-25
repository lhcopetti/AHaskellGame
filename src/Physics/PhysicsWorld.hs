module Physics.PhysicsWorld
    ( initPhysicsLibrary
    , createWorld
    , stepWorld
    , getCollisionData
    ) where

import qualified Physics.Library.Hipmunk.HipmunkWorld       as HMP -- Hipmunk Physics
import qualified Physics.Library.Hipmunk.PhysicsTypes       as HMP
import qualified Physics.Library.Hipmunk.HipmunkCollision   as HMP

import Physics.PhysicsTypes (PhysicsWorld)

initPhysicsLibrary :: IO ()
initPhysicsLibrary = HMP.initPhysicsLibrary

createWorld :: Float -> IO PhysicsWorld
createWorld = HMP.createWorld

stepWorld :: Float -> PhysicsWorld -> IO ()
stepWorld = HMP.stepWorld

getCollisionData :: PhysicsWorld -> IO HMP.PhyCollisionData
getCollisionData = HMP.getCollisionData