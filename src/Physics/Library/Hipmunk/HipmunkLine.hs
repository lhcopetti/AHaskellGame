{-# LANGUAGE NamedFieldPuns #-}
module Physics.Library.Hipmunk.HipmunkLine
    ( mkLinePhysics
    ) where

import qualified Physics.Hipmunk as H
import Data.StateVar
import GameObject.GameObjectTypes (Physics (..))
import Physics.Library.Hipmunk.PhysicsTypes

mkLinePhysics  :: (H.Vector, H.Vector) -> Double -> PhysicsWorld -> IO Physics
mkLinePhysics (start, end) thickness PhysicsWorld { space } = do
    let t = H.LineSegment start end thickness
    body <- H.newBody H.infinity H.infinity
    shape <- H.newShape body t 0
    ----
    H.friction shape $= 1.0
    H.elasticity shape $= 0.6
    ----
    -- H.spaceAdd space body
    -- Don't add the body for static shapes
    H.spaceAdd space (H.Static shape)
    ----
    let remove = H.spaceRemove space (H.Static shape)
                -- We don't need to remove the body because all line shapes are currently being
                -- created as static objects
    return (LibraryPhy $ PL body shape t remove)