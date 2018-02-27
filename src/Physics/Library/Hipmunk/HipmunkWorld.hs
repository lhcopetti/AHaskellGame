module Physics.Library.Hipmunk.HipmunkWorld
    ( initPhysicsLibrary
    , createWorld
    , stepWorld
    ) where

import qualified Physics.Hipmunk as H

import Physics.Library.Hipmunk.PhysicsTypes (PhysicsWorld (..))
import Data.StateVar

import NativeResource

initPhysicsLibrary :: IO ()
initPhysicsLibrary = H.initChipmunk

createWorld :: Float -> IO PhysicsWorld
createWorld gravity = do
    space <- H.newSpace
    H.gravity space $= H.Vector 0 (realToFrac gravity)
    return (PhysicsWorld space)

instance NativeResource PhysicsWorld where
    free (PhysicsWorld space) = H.freeSpace space

stepWorld :: Float -> PhysicsWorld -> IO ()
stepWorld delta (PhysicsWorld sp) = H.step sp (realToFrac delta)