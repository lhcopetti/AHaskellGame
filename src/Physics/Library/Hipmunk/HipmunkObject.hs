module Physics.Library.Hipmunk.HipmunkObject
    ( updateObjectPhysics
    ) where

import qualified Physics.Hipmunk as H

import Control.Monad (liftM)

        
import GameObject.GameObjectTypes
import GameObject.GameObject ()
import Physics.Library.Hipmunk.VectorConversion (hVectorToVec2f)
import Component.Position
import Data.StateVar


updateObjectPhysics :: GameObject -> IO GameObject
updateObjectPhysics go = 
    case physicsComp go of 
        (HipPhy b _ _ _) -> do 
            position <- liftM hVectorToVec2f   . get . H.position  $ b
            angle <-    liftM realToFrac       . get . H.angle     $ b
            putStrLn $ "This is the position" ++ show position ++ show angle
            liftM ((`setPosition` position) . setRotation angle) (return go)
        _ -> return go