module Physics.LinePhysics
    ( mkLinePhysics
    , mkLinePhysicsD
    ) where

import SFML.System.Vector2 (Vec2f)

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import qualified Physics.Library.Hipmunk.HipmunkLine as HMP
import GameObject.GameObjectTypes
import Physics.PhysicsTypes (PhysicsWorld)
import Physics.Library.Hipmunk.VectorConversion (vec2fToHVector)
import Physics.DebugDraw.DebugDraw (mkDebugDraw)

mkLinePhysics :: (Vec2f, Vec2f) -> Float -> PhysicsWorld -> IO Physics
mkLinePhysics (start, end) thickness = 
    HMP.mkLinePhysics (start', end') thickness'
        where
            start'      = vec2fToHVector start
            end'        = vec2fToHVector end
            thickness'  = realToFrac thickness

mkLinePhysicsD :: (Vec2f, Vec2f) -> Float -> PhysicsWorld -> MaybeT IO (Physics, Drawing)
mkLinePhysicsD line thickness world = do
    physics <- liftIO $ mkLinePhysics line thickness world
    draw    <- mkDebugDraw physics
    return (physics, draw)