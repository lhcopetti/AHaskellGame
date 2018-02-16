module Physics.PhysicsObject
    ( updateObjectPhysics
    ) where

import qualified Physics.Library.Hipmunk.HipmunkObject as HMP -- Hipmunk Physics
import Physics.PhysicsTypes

import Component.Position

updateObjectPhysics :: Position a => PhyObject -> a -> IO a
updateObjectPhysics = HMP.updateObjectPhysics