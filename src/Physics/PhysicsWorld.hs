module Physics.PhysicsWorld
    ( initPhysicsLibrary
    , createWorld
    , stepWorld
    ) where

import qualified Physics.Library.Hipmunk.HipmunkWorld as HMP -- Hipmunk Physics

import Physics.PhysicsTypes (PhysicsWorld)

initPhysicsLibrary :: IO ()
initPhysicsLibrary = HMP.initPhysicsLibrary

createWorld :: Float -> IO PhysicsWorld
createWorld = HMP.createWorld

stepWorld :: Float -> PhysicsWorld -> IO ()
stepWorld = HMP.stepWorld