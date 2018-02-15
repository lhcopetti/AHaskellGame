{-# LANGUAGE RecordWildCards #-}
module Physics.Library.Hipmunk.HipmunkObject
    ( updateObjectPhysics
    ) where

import qualified Physics.Hipmunk as H

import Control.Monad (liftM)

import GameObject.GameObject ()
import Physics.Library.Hipmunk.PhysicsTypes
import Physics.Library.Hipmunk.VectorConversion (hVectorToVec2f)
import Component.Position
import Data.StateVar


updateObjectPhysics :: Position a => PhysicsObject -> a -> IO a
updateObjectPhysics PL{..} go =
        do  position <- liftM hVectorToVec2f . get . H.position $ body
            angle    <- liftM realToFrac     . get . H.angle    $ body
            putStrLn $ "This is the position" ++ show position ++ show angle
            liftM ((`setPosition` position) . setRotation angle) (return go)