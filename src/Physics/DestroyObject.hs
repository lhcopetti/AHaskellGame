module Physics.DestroyObject
    ( destroyPhysics
    ) where

import qualified Physics.Library.Hipmunk.DestroyObject as HMP

import GameObject.GameObjectTypes


destroyPhysics :: Physics -> IO ()
destroyPhysics = HMP.destroyPhysics