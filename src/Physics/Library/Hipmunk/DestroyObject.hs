module Physics.Library.Hipmunk.DestroyObject
    ( destroyPhysics
    ) where

import GameObject.GameObjectTypes


destroyPhysics :: Physics -> IO ()
destroyPhysics phy = 
    case phy of 
        (HipPhy (PL _ _ _ destroyCallback)) -> destroyCallback
        _ -> return ()