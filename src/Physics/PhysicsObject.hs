module Physics.PhysicsObject
    ( updateObjectPhysics
    ) where

import qualified Physics.Library.Hipmunk.HipmunkObject as HMP -- Hipmunk Physics

import GameObject.GameObjectTypes (GameObject)

updateObjectPhysics :: GameObject -> IO GameObject
updateObjectPhysics = HMP.updateObjectPhysics