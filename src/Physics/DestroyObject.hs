module Physics.DestroyObject
    ( destroyPhysics
    ) where

import qualified Physics.Library.Hipmunk.DestroyObject as HMP

import GameObject.GameObjectTypes


destroyPhysics :: Physics -> IO ()
destroyPhysics phy = case phy of 
    LibraryPhy x    -> HMP.destroyPhysics x
    _               -> return ()