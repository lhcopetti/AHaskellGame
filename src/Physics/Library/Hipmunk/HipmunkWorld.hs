module Physics.Library.Hipmunk.HipmunkWorld
    ( createWorld
    , stepWorld
    ) where

import qualified Physics.Hipmunk as H

import Physics.PhysicsTypes (PhysicsWorld (..))
import Data.StateVar

createWorld :: Float -> IO PhysicsWorld
createWorld gravity = do
    space <- H.newSpace
    H.gravity space $= H.Vector 0 (realToFrac gravity)
    return (PhysicsWorld space)

stepWorld :: Float -> PhysicsWorld -> IO ()
stepWorld delta (PhysicsWorld sp) = H.step sp (realToFrac delta)