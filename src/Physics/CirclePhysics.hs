module Physics.CirclePhysics
    ( mkCirclePhysics
    , mkCirclePhysicsD
    ) where

import SFML.System.Vector2 (Vec2f)

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import qualified Physics.Library.Hipmunk.HipmunkCircle as HMP
import GameObject.GameObjectTypes
import Physics.PhysicsTypes (PhysicsWorld)
import Physics.Library.Hipmunk.VectorConversion (vec2fToHVector)
import Physics.Library.DebugDraw.DebugDraw (mkDebugDraw)

mkCirclePhysics :: Float -> Vec2f -> PhysicsWorld -> IO Physics
mkCirclePhysics radius pos = HMP.mkCirclePhysics hRadius hPos
    where
        hRadius = realToFrac radius
        hPos    = vec2fToHVector pos
        
mkCirclePhysicsD :: Float -> Vec2f -> PhysicsWorld -> MaybeT IO (Physics, Drawing)
mkCirclePhysicsD radius pos world = do
    physics <- liftIO $ mkCirclePhysics radius pos world
    draw    <- mkDebugDraw physics
    return (physics, draw)