module Physics.LinePhysics
    ( mkLinePhysics
    ) where

import SFML.System.Vector2 (Vec2f)

import qualified Physics.Library.Hipmunk.HipmunkLine as HMP
import GameObject.GameObjectTypes (Physics (..))
import Physics.PhysicsTypes (PhysicsWorld)
import Physics.Library.Hipmunk.VectorConversion (vec2fToHVector)

mkLinePhysics :: (Vec2f, Vec2f) -> Float -> PhysicsWorld -> IO Physics
mkLinePhysics (start, end) thickness = 
    HMP.mkLinePhysics (start', end') thickness'
        where
            start'      = vec2fToHVector start
            end'        = vec2fToHVector end
            thickness'  = realToFrac thickness