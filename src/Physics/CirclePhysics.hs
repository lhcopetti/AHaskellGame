module Physics.CirclePhysics
    ( mkCirclePhysics
    ) where

import SFML.System.Vector2 (Vec2f)

import qualified Physics.Library.Hipmunk.HipmunkCircle as HMP
import GameObject.GameObjectTypes (Physics (..))
import Physics.PhysicsTypes (PhysicsWorld)
import Physics.Library.Hipmunk.VectorConversion (vec2fToHVector)

mkCirclePhysics :: Float -> Vec2f -> PhysicsWorld -> IO Physics
mkCirclePhysics radius pos = HMP.mkCirclePhysics hRadius hPos
    where
        hRadius = realToFrac radius
        hPos    = vec2fToHVector pos
        