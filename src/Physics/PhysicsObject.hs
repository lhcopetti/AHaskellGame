module Physics.PhysicsObject
    ( updateObjectPhysics
    ) where

import qualified Physics.Library.Hipmunk.HipmunkObject as HMP -- Hipmunk Physics

import GameObject.GameObjectTypes

updateObjectPhysics :: GameObject -> IO GameObject
updateObjectPhysics go = case physicsComp go of
            (HipPhy pl) -> HMP.updateObjectPhysics pl go
            _           -> return go