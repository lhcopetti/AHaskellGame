module Physics.Library.Hipmunk.HipmunkWorld
    ( initPhysicsLibrary
    , createWorld
    , stepWorld
    ) where

import qualified Physics.Hipmunk as H

import Control.Monad.IO.Class (liftIO)
import Data.IORef

import Physics.Library.Hipmunk.PhysicsTypes
import Physics.Library.Hipmunk.HipmunkCollision
import Data.StateVar

import NativeResource

initPhysicsLibrary :: IO ()
initPhysicsLibrary = H.initChipmunk

createWorld :: Float -> IO PhysicsWorld
createWorld gravity = do
    space <- H.newSpace
    H.gravity space $= H.Vector 0 (realToFrac gravity)
    collData <- newIORef emptyCollisionData
    H.setDefaultCollisionHandler space (defaultHandler collData)
    return (PhysicsWorld space collData)

defaultHandler :: IORef PhyCollisionData -> H.CollisionHandler
defaultHandler dataRef = 
    H.Handler  { H.beginHandler      = Nothing                     -- Library default
                , H.preSolveHandler  = Nothing                     -- Library default
                , H.postSolveHandler = Just (phyCallback dataRef)
                , H.separateHandler  = Nothing                     -- Library default
                }

phyCallback :: (H.NotSeparate ns) => IORef PhyCollisionData -> H.Callback ns ()
phyCallback collData = do
    ps <- H.points
    ss <- H.shapes
    liftIO $ modifyIORef collData (appendCollData (ss, ps))

instance NativeResource PhysicsWorld where
    free = H.freeSpace . space

stepWorld :: Float -> PhysicsWorld -> IO ()
stepWorld delta world = do
    clearCollisionInformation world
    H.step (space world) (realToFrac delta)

clearCollisionInformation :: PhysicsWorld -> IO ()
clearCollisionInformation = (`writeIORef` emptyCollisionData) . collCallback