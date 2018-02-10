module Physics.PhysicsWorld
    ( createWorld
    , stepWorld
    ) where

import qualified Physics.Library.Hipmunk.HipmunkWorld as HMP -- Hipmunk Physics

import Physics.PhysicsTypes (PhysicsWorld)

createWorld :: Float -> IO PhysicsWorld
createWorld = HMP.createWorld

stepWorld :: Float -> PhysicsWorld -> IO ()
stepWorld = HMP.stepWorld